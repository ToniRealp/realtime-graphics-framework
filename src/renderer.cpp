#include "renderer.h"

#include <algorithm>

#include "application.h"
#include "camera.h"
#include "fbo.h"
#include "shader.h"
#include "mesh.h"
#include "texture.h"
#include "prefab.h"
#include "material.h"
#include "utils.h"
#include "scene.h"
#include "extra/hdre.h"


using namespace GTR;

Renderer::Renderer()
{
	gbuffers_fbo = nullptr;
	illumination_fbo = nullptr;
	ambient_occlusion_fbo = nullptr;
	irr_fbo = nullptr;
	probes_texture = nullptr;

	use_single_pass = false;
	debug_gbuffers = false;
	render_to_full_screen_quad = false;
	debug_ssao = false;
	debug_probes_texture = false;
	render_pipeline = DEFERRED;

	random_points = generateSpherePoints(num_points, 1, false);
}


void Renderer::renderScene(GTR::Scene* scene, Camera* camera)
{
	lights.clear();
	render_calls.clear();
	//set the clear color (the background color)

	//render entities
	for (int i = 0; i < scene->entities.size(); ++i)
	{
		BaseEntity* ent = scene->entities[i];

		if (!ent->visible)
			continue;

		if (ent->entity_type == LIGHT)
		{
			lights.push_back(dynamic_cast<GTR::LightEntity*>(ent));
		}
		else if (ent->entity_type == PREFAB)
		{
			const auto pent = dynamic_cast<GTR::PrefabEntity*>(ent);
			if (pent->prefab)
				renderPrefab(ent->model, pent->prefab, camera);
		}
	}

	std::sort(render_calls.begin(), render_calls.end(), [](render_call lhs, render_call rhs)
	{
		if (lhs.material->alpha_mode == GTR::eAlphaMode::NO_ALPHA && rhs.material->alpha_mode ==
			GTR::eAlphaMode::NO_ALPHA)
		{
			return lhs.distance_to_camera < rhs.distance_to_camera;
		}
		else if (lhs.material->alpha_mode == GTR::eAlphaMode::NO_ALPHA && rhs.material->alpha_mode ==
			GTR::eAlphaMode::BLEND)
		{
			return true;
		}
		else if (lhs.material->alpha_mode == GTR::eAlphaMode::BLEND && rhs.material->alpha_mode ==
			GTR::eAlphaMode::NO_ALPHA)
		{
			return false;
		}
		else
		{
			return lhs.distance_to_camera < rhs.distance_to_camera;
		}
	});


	switch (render_pipeline) {
		case FORWARD:
			for (const auto& light : lights)
			{
				generate_shadow_map(light);
			}
			render_forward(camera, scene);
			break;
		case DEFERRED:
			for (const auto& light : lights)
			{
				generate_shadow_map(light);
			}
			render_deferred(camera, scene);
			break;
	}


	// glViewport(0, 0, 256, 256);
	// show_shadowmap(lights[0]);
	// glViewport(0, 0, Application::instance->window_width, Application::instance->window_height);
}

//renders all the prefab
void Renderer::renderPrefab(const Matrix44& model, GTR::Prefab* prefab, Camera* camera)
{
	assert(prefab && "PREFAB IS NULL");
	//assign the model to the root node
	renderNode(model, &prefab->root, camera);
}

//renders a node of the prefab and its children
void Renderer::renderNode(const Matrix44& prefab_model, GTR::Node* node, Camera* camera)
{
	if (!node->visible)
		return;

	//compute global matrix
	Matrix44 node_model = node->getGlobalMatrix(true) * prefab_model;

	//does this node have a mesh? then we must render it
	if (node->mesh && node->material)
	{
		//compute the bounding box of the object in world space (by using the mesh bounding box transformed to world space)
		BoundingBox world_bounding = transformBoundingBox(node_model, node->mesh->box);
		createRenderCall(node_model, node->mesh, node->material, camera, world_bounding);
	}

	//iterate recursively with children
	for (int i = 0; i < node->children.size(); ++i)
		renderNode(prefab_model, node->children[i], camera);
}

void Renderer::createRenderCall(const Matrix44 model, Mesh* mesh, GTR::Material* material, Camera* camera,
                                BoundingBox world_bounding)
{
	Vector3 node_position = model * Vector3();
	float distance = node_position.distance(camera->eye);
	render_calls.emplace_back(mesh, material, model, distance, world_bounding);
}

void Renderer::render_flat_mesh(const Matrix44 model, Mesh* mesh, GTR::Material* material, Camera* camera)
{
	//in case there is nothing to do
	if (!mesh || !mesh->getNumVertices() || !material)
		return;
	assert(glGetError() == GL_NO_ERROR);

	//define locals to simplify coding
	Shader* shader;

	//select if render both sides of the triangles
	if (material->two_sided)
		glDisable(GL_CULL_FACE);
	else
		glEnable(GL_CULL_FACE);
	assert(glGetError() == GL_NO_ERROR);

	//chose a shader
	shader = Shader::Get("flat");

	assert(glGetError() == GL_NO_ERROR);

	//no shader? then nothing to render
	if (!shader)
		return;
	shader->enable();


	shader->setUniform("u_viewprojection", camera->viewprojection_matrix);

	shader->setUniform("u_model", model);

	//this is used to say which is the alpha threshold to what we should not paint a pixel on the screen (to cut polygons according to texture alpha)
	shader->setUniform("u_alpha_cutoff", material->alpha_mode == GTR::eAlphaMode::MASK ? material->alpha_cutoff : 0);


	glDepthFunc(GL_LESS);
	glDisable(GL_BLEND);
	mesh->render(GL_TRIANGLES);

	//disable shader
	shader->disable();
}

Texture* GTR::CubemapFromHDRE(const char* filename)
{
	HDRE* hdre = HDRE::Get(filename);
	if (!hdre)
		return NULL;

	auto texture = new Texture();
	if (hdre->getFacesf(0))
	{
		texture->createCubemap(hdre->width, hdre->height, (Uint8**)hdre->getFacesf(0),
		                       hdre->header.numChannels == 3 ? GL_RGB : GL_RGBA, GL_FLOAT);
		for (int i = 1; i < hdre->levels; ++i)
			texture->uploadCubemap(texture->format, texture->type, false,
			                       (Uint8**)hdre->getFacesf(i), GL_RGBA32F, i);
	}
	else if (hdre->getFacesh(0))
	{
		texture->createCubemap(hdre->width, hdre->height, (Uint8**)hdre->getFacesh(0),
		                       hdre->header.numChannels == 3 ? GL_RGB : GL_RGBA, GL_HALF_FLOAT);
		for (int i = 1; i < hdre->levels; ++i)
			texture->uploadCubemap(texture->format, texture->type, false,
			                       (Uint8**)hdre->getFacesh(i), GL_RGBA16F, i);
	}
	return texture;
}

void GTR::Renderer::generate_shadow_map(LightEntity* light)
{
	if (light->light_type != spot)
		return;
	if (!light->cast_shadows)
	{
		if (light->fbo)
		{
			delete light->fbo;
			light->fbo = nullptr;
			light->shadowmap = nullptr;
		}
		return;
	}

	if (!light->fbo)
	{
		light->fbo = new FBO();
		light->fbo->setDepthOnly(1024, 1024);
		light->shadowmap = light->fbo->depth_texture;
	}

	if (!light->camera)
	{
		light->camera = new Camera();
	}

	light->fbo->bind();
	glClear(GL_DEPTH_BUFFER_BIT);

	Camera* view_camera = Camera::current;

	Camera* light_camera = light->camera;
	light_camera->lookAt(light->model.getTranslation(), light->model * Vector3(0, 0, -1),
	                     light->model.rotateVector(Vector3(0, 1, 0)));
	light_camera->setPerspective(light->cone_angle, 1, 0.1f, light->max_distance);
	light_camera->enable();


	for (const auto& rc : render_calls)
	{
		if (rc.material->alpha_mode == BLEND)
			continue;
		if (light_camera->testBoxInFrustum(rc.world_bounding.center, rc.world_bounding.halfsize))
			render_flat_mesh(rc.model, rc.mesh, rc.material, light_camera);
	}

	light->fbo->unbind();

	view_camera->enable();
}

void GTR::Renderer::show_shadowmap(LightEntity* light)
{
	Shader* shader = Shader::getDefaultShader("depth");
	shader->enable();
	shader->setUniform("u_camera_nearfar", Vector2(light->camera->near_plane, light->camera->far_plane));
	lights[0]->shadowmap->toViewport(shader);
}

void Renderer::renderProbe(Vector3 pos, float size, float* coeffs)
{
	Camera* camera = Camera::current;
	Shader* shader = Shader::Get("probe");
	Mesh* mesh = Mesh::Get("data/meshes/sphere.obj");

	glEnable(GL_CULL_FACE);
	glDisable(GL_BLEND);
	glEnable(GL_DEPTH_TEST);

	Matrix44 model;
	model.setTranslation(pos.x, pos.y, pos.z);
	model.scale(size, size, size);

	shader->enable();
	shader->setUniform("u_viewprojection",
camera->viewprojection_matrix);
	shader->setUniform("u_camera_position", camera->eye);
	shader->setUniform("u_model", model);
	shader->setUniform3Array("u_coeffs", coeffs, 9);

	mesh->render(GL_TRIANGLES);
}

void Renderer::captureProbe(sProbe &probe, Scene* scene)
{
	if (irr_fbo == nullptr)
	{
		irr_fbo = new FBO();
		irr_fbo->create(64,64,1, GL_RGB, GL_FLOAT);
	}
	
	FloatImage images[6]; //here we will store the six views
	Camera cam;

	//set the fov to 90 and the aspect to 1
	cam.setPerspective(90, 1, 0.1, 1000);

	for (int i = 0; i < 6; ++i) //for every cubemap face
		{
		//compute camera orientation using defined vectors
		Vector3 eye = probe.pos;
		Vector3 front = cubemapFaceNormals[i][2];
		Vector3 center = probe.pos + front;
		Vector3 up = cubemapFaceNormals[i][1];
		cam.lookAt(eye, center, up);
		cam.enable();

		//render the scene from this point of view
		irr_fbo->bind();
		render_forward(&cam, scene);
		irr_fbo->unbind();

		//read the pixels back and store in a FloatImage
		images[i].fromTexture( irr_fbo->color_textures[0] );
		}

	//compute the coefficients given the six images
	probe.sh = computeSH(images);
}

void Renderer::generateProbes(Scene* scene)
{
	probes.clear();
	Vector3 start_pos(-300, 5, -400);
	Vector3 end_pos(300, 150, 400);

	//define how many probes you want per dimension
	Vector3 dim(10, 4, 10);

	//compute the vector from one corner to the other
	Vector3 delta = (end_pos - start_pos);

	//and scale it down according to the subdivisions
	//we substract one to be sure the last probe is at end pos
	delta.x /= (dim.x - 1);
	delta.y /= (dim.y - 1);
	delta.z /= (dim.z - 1);

	for (int z = 0; z < dim.z; ++z)
		for (int y = 0; y < dim.y; ++y)
			for (int x = 0; x < dim.x; ++x)
			{
				sProbe p;
				p.local.set(x, y, z);

				//index in the linear array
				p.index = x + y * dim.x + z * dim.x * dim.y;

				//and its position
				p.pos = start_pos + delta * Vector3(x,y,z);
				probes.push_back(p);
			}


	std::cout<<"Generating probes"<<std::endl;
	for (auto & probe : probes)
	{
		captureProbe(probe, scene);
		std::cout<<".";
	}
	std::cout<<"Done"<<std::endl;

	delete probes_texture;
	
	probes_texture = new Texture( 
	9, //9 coefficients per probe
	probes.size(), //as many rows as probes
	GL_RGB, //3 channels per coefficient
	GL_FLOAT ); //they require a high range

	//we must create the color information for the texture. because every SH are 27 floats in the RGB,RGB,... order, we can create an array of SphericalHarmonics and use it as pixels of the texture
	SphericalHarmonics* sh_data = NULL;
	sh_data = new SphericalHarmonics[ dim.x * dim.y * dim.z ];

	//here we fill the data of the array with our probes in x,y,z order
	for (int i = 0; i < probes.size(); ++i)
		sh_data[i] = probes[i].sh;

	//now upload the data to the GPU as a texture
	probes_texture->upload( GL_RGB, GL_FLOAT, false, (uint8*)sh_data);

	//disable any texture filtering when reading
	probes_texture->bind();
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

	//always free memory after allocating it!!!
	delete[] sh_data;


}


void Renderer::upload_light_to_shader(Shader* shader, const LightEntity*  light)
{
	shader->setUniform("u_light_type", light->light_type);

	shader->setUniform("u_light_color", light->color * light->intensity);
	shader->setUniform("u_light_position", light->model * Vector3());
	shader->setUniform("u_light_max_distance", light->max_distance);

	shader->setUniform("u_light_cone_angle", static_cast<float>(cos(light->cone_angle * DEG2RAD)));
	shader->setUniform("u_light_exp", light->cone_exp);
	shader->setUniform("u_light_direction", light->model.rotateVector(Vector3(0, 0, 1)));


	if (light->shadowmap)
	{
		shader->setUniform("u_light_casts_shadows", 1);
		shader->setUniform("u_light_shadowmap", light->shadowmap, 8);
		shader->setUniform("u_shadow_viewproj", light->camera->viewprojection_matrix);
		shader->setUniform("u_shadow_bias", light->shadow_bias);

	}
	else
		shader->setUniform("u_light_casts_shadows", 0);
}

void Renderer::render_mesh_with_material_single_pass(const Matrix44& model, Mesh* mesh, Material* material, Camera* camera)
{
	//in case there is nothing to do
	if (!mesh || !mesh->getNumVertices() || !material)
		return;
	assert(glGetError() == GL_NO_ERROR);

	//define locals to simplify coding
	Shader* shader = nullptr;
	Texture* texture = nullptr;
	const GTR::Scene* scene = GTR::Scene::instance;

	const int num_lights = lights.size();

	texture = material->color_texture.texture;
	//texture = material->emissive_texture;
	//texture = material->metallic_roughness_texture;
	//texture = material->normal_texture;
	//texture = material->occlusion_texture;
	if (texture == nullptr)
		texture = Texture::getWhiteTexture(); //a 1x1 white texture

	if (material->alpha_mode == GTR::eAlphaMode::BLEND)
	{
		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	}
	else
		glDisable(GL_BLEND);

	//select if render both sides of the triangles
	if (material->two_sided)
		glDisable(GL_CULL_FACE);
	else
		glEnable(GL_CULL_FACE);
	assert(glGetError() == GL_NO_ERROR);

	//chose a shader
	shader = Shader::Get("single");

	assert(glGetError() == GL_NO_ERROR);

	//no shader? then nothing to render
	if (!shader)
		return;
	shader->enable();

	//upload uniforms
	float t = getTime();

	shader->setUniform("u_viewprojection", camera->viewprojection_matrix);
	shader->setUniform("u_camera_position", camera->eye);
	shader->setUniform("u_model", model);
	shader->setUniform("u_time", t);

	shader->setUniform("u_color", material->color);
	if (texture) shader->setUniform("u_texture", texture, 0);

	//this is used to say which is the alpha threshold to what we should not paint a pixel on the screen (to cut polygons according to texture alpha)
	shader->setUniform("u_alpha_cutoff", material->alpha_mode == GTR::eAlphaMode::MASK ? material->alpha_cutoff : 0);

	shader->setUniform("u_ambient_light", scene->ambient_light);

	//glDepthFunc(GL_LEQUAL);
	//glBlendFunc(GL_SRC_ALPHA, GL_ONE);

	constexpr int max_lights = 10;

	int lights_type[max_lights];

	Vector3 lights_position[max_lights];
	Vector3 lights_color[max_lights];
	float lights_max_distance[max_lights];

	float lights_cone_angle[max_lights];
	float lights_cone_exp[max_lights];
	Vector3 lights_direction[max_lights];

	int lights_cast_shadow[max_lights];


	for (int i=0; i<lights.size(); i++)
	{
		LightEntity* light = lights[i];
		lights_type[i] = light->light_type;

		lights_color[i] = light->color * light->intensity;
		lights_position[i] = light->model * Vector3();
		lights_max_distance[i] = light->max_distance;

		lights_cone_angle[i] = static_cast<float>(cos(light->cone_angle * DEG2RAD));
		lights_cone_exp[i] = light->cone_exp;
		lights_direction[i] = light->model.rotateVector(Vector3(0, 0, 1));
	}

	shader->setUniform("num_lights", static_cast<int>(lights.size()));

	shader->setUniform1Array("u_lights_type",reinterpret_cast<int*>(&lights_type), max_lights);

	shader->setUniform3Array("u_lights_color",(float*) & lights_color, max_lights);
	shader->setUniform3Array("u_lights_position", (float*)&lights_position, max_lights);
	shader->setUniform1Array("u_lights_max_distance", (float*)&lights_max_distance, max_lights);

	shader->setUniform1Array("u_lights_cone_angle", reinterpret_cast<float*>(&lights_cone_angle), max_lights);
	shader->setUniform1Array("u_lights_exp", reinterpret_cast<float*>(&lights_cone_exp), max_lights);
	shader->setUniform3Array("u_lights_direction", reinterpret_cast<float*>(&lights_direction), max_lights);


	shader->setUniform("u_light_casts_shadows", 0);

	mesh->render(GL_TRIANGLES);
	glEnable(GL_BLEND);
	shader->setUniform("u_ambient_light", Vector3());


	glDisable(GL_BLEND);
	glDepthFunc(GL_LESS);

	//disable shader
	shader->disable();

	//set the render state as it was before to avoid problems with future renders
	glDisable(GL_BLEND);
}

void Renderer::render_mesh_with_material_and_lighting(const Matrix44 model, Mesh* mesh, GTR::Material* material, Camera* camera)
{
	//in case there is nothing to do
	if (!mesh || !mesh->getNumVertices() || !material)
		return;
	assert(glGetError() == GL_NO_ERROR);

	//define locals to simplify coding
	Shader* shader = nullptr;
	const GTR::Scene* scene = GTR::Scene::instance;

	const int num_lights = lights.size();

	Texture* texture = material->color_texture.texture;
	Texture* emissive_texture = material->emissive_texture.texture;
	//texture = material->emissive_texture;
	//texture = material->metallic_roughness_texture;
	//texture = material->normal_texture;
	//texture = material->occlusion_texture;
	if (texture == nullptr)
		texture = Texture::getWhiteTexture(); //a 1x1 white texture
	if(!emissive_texture)
		emissive_texture = Texture::getWhiteTexture();

	if (material->alpha_mode == GTR::eAlphaMode::BLEND)
	{
		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	}
	else
		glDisable(GL_BLEND);

	//select if render both sides of the triangles
	if (material->two_sided)
		glDisable(GL_CULL_FACE);
	else
		glEnable(GL_CULL_FACE);
	assert(glGetError() == GL_NO_ERROR);

	//chose a shader
	shader = Shader::Get("light");

	assert(glGetError() == GL_NO_ERROR);

	//no shader? then nothing to render
	if (!shader)
		return;
	shader->enable();

	//upload uniforms
	float t = getTime();

	shader->setUniform("u_viewprojection", camera->viewprojection_matrix);
	shader->setUniform("u_camera_position", camera->eye);
	shader->setUniform("u_model", model);
	shader->setUniform("u_time", t);

	shader->setUniform("u_color", material->color);
	if (texture) shader->setUniform("u_texture", texture, 0);

	shader->setUniform("u_emissive", material->emissive_factor);
	if (emissive_texture)
		shader->setUniform("u_emissive_texture", emissive_texture, 1);

	//this is used to say which is the alpha threshold to what we should not paint a pixel on the screen (to cut polygons according to texture alpha)
	shader->setUniform("u_alpha_cutoff", material->alpha_mode == GTR::eAlphaMode::MASK ? material->alpha_cutoff : 0);

	shader->setUniform("u_ambient_light", scene->ambient_light);

	glDepthFunc(GL_LEQUAL);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE);


	for (const auto& light : lights)
	{
		upload_light_to_shader(shader, light);
		mesh->render(GL_TRIANGLES);
		glEnable(GL_BLEND);
		shader->setUniform("u_ambient_light", Vector3());
	}


	glDisable(GL_BLEND);
	glDepthFunc(GL_LESS);

	//disable shader
	shader->disable();
	
}

void Renderer::render_mesh_with_material_to_gbuffer(const Matrix44 model, Mesh* mesh, GTR::Material* material, Camera* camera)
{
	//in case there is nothing to do
	if (!mesh || !mesh->getNumVertices() || !material)
		return;
	assert(glGetError() == GL_NO_ERROR);

	if (material->alpha_mode == BLEND)
		return;
	

	//define locals to simplify coding
	Shader* shader = nullptr;
	const GTR::Scene* scene = GTR::Scene::instance;

	Texture* texture = material->color_texture.texture;
	Texture* emissive_texture = material->emissive_texture.texture;
	Texture* metallic_roughness_texture = material->metallic_roughness_texture.texture;
	//texture = material->normal_texture;
	//texture = material->occlusion_texture;
	if (texture == nullptr)
		texture = Texture::getWhiteTexture(); //a 1x1 white texture
	if(!emissive_texture)
		emissive_texture = Texture::getWhiteTexture();
	if(!metallic_roughness_texture)
		metallic_roughness_texture = Texture::getWhiteTexture();
	//select if render both sides of the triangles
	
	if (material->two_sided)
		glDisable(GL_CULL_FACE);
	else
		glEnable(GL_CULL_FACE);
	assert(glGetError() == GL_NO_ERROR);

	//chose a shader
	shader = Shader::Get("gbuffers");

	assert(glGetError() == GL_NO_ERROR);

	//no shader? then nothing to render
	if (!shader)
		return;
	shader->enable();

	//upload uniforms
	float t = getTime();

	shader->setUniform("u_viewprojection", camera->viewprojection_matrix);
	shader->setUniform("u_camera_position", camera->eye);
	shader->setUniform("u_model", model);
	shader->setUniform("u_time", t);

	shader->setUniform("u_color", material->color);
	if (texture)
		shader->setUniform("u_texture", texture, 0);

	shader->setUniform("u_emissive", material->emissive_factor);
	if (emissive_texture)
		shader->setUniform("u_emissive_texture", emissive_texture, 1);
	
	if (metallic_roughness_texture)
		shader->setUniform("u_metallic_roughness_texture", metallic_roughness_texture, 2);
	

	//this is used to say which is the alpha threshold to what we should not paint a pixel on the screen (to cut polygons according to texture alpha)
	shader->setUniform("u_alpha_cutoff", material->alpha_mode == GTR::eAlphaMode::MASK ? material->alpha_cutoff : 0);

	mesh->render(GL_TRIANGLES);

	glDisable(GL_BLEND);
	glDepthFunc(GL_LESS);

	//disable shader
	shader->disable();
}

void Renderer::render_gbuffers_with_ambient(Camera* camera, Scene* scene)
{

	glDisable(GL_DEPTH_TEST);
	glDisable(GL_BLEND);


	Mesh* mesh = Mesh::getQuad();
	Shader* shader  = Shader::Get("ambient");
	shader->enable();

	shader->setUniform("u_ambient_light", scene->ambient_light);


	shader->setUniform("u_gb0_texture", gbuffers_fbo->color_textures[0], 0);
	shader->setUniform("u_ssao_texture", ambient_occlusion_fbo->color_textures[0], 4);
	
	
	shader->setUniform("u_iRes", Vector2(1.0 / static_cast<float>(Application::instance->window_width),
										 1.0 / static_cast<float>(Application::instance->window_height)));

	
	mesh->render(GL_TRIANGLES);
	
}

void Renderer::render_gbuffers_with_illumination_quad(Camera* camera, Scene* scene)
{
	glDisable(GL_DEPTH_TEST);

	Mesh* mesh = Mesh::getQuad();
	Shader* shader  = Shader::Get("deferred_PBR");
	shader->enable();
	
	shader->setUniform("u_camera_position", camera->eye);

	shader->setUniform("u_gb0_texture", gbuffers_fbo->color_textures[0], 0);
	shader->setUniform("u_gb1_texture", gbuffers_fbo->color_textures[1], 1);
	shader->setUniform("u_gb2_texture", gbuffers_fbo->color_textures[2], 2);
	shader->setUniform("u_depth_texture", gbuffers_fbo->depth_texture, 3);


	Matrix44 inverse_view_projection = camera->viewprojection_matrix;
	inverse_view_projection.inverse();
	
	shader->setUniform("u_inverse_viewprojection", inverse_view_projection);
	shader->setUniform("u_iRes", Vector2(1.0 / static_cast<float>(Application::instance->window_width),
										 1.0 / static_cast<float>(Application::instance->window_height)));

	
	glEnable(GL_BLEND);
	glBlendFunc(GL_ONE, GL_ONE);
	
	
	for (const auto& light : lights)
	{
		upload_light_to_shader(shader, light);
		mesh->render(GL_TRIANGLES);
		
	}
}

void Renderer::render_gbuffers_with_illumination_geometry(Camera* camera, Scene* scene)
{
	glDisable(GL_DEPTH_TEST);

	Mesh* mesh = Mesh::Get("data/meshes/sphere.obj");
	Shader* shader  = Shader::Get("deferred_ws");
	shader->enable();
	
	shader->setUniform("u_camera_position", camera->eye);

	shader->setUniform("u_gb0_texture", gbuffers_fbo->color_textures[0], 0);
	shader->setUniform("u_gb1_texture", gbuffers_fbo->color_textures[1], 1);
	shader->setUniform("u_gb2_texture", gbuffers_fbo->color_textures[2], 2);
	shader->setUniform("u_depth_texture", gbuffers_fbo->depth_texture, 3);


	Matrix44 inverse_view_projection = camera->viewprojection_matrix;
	inverse_view_projection.inverse();
	
	shader->setUniform("u_inverse_viewprojection", inverse_view_projection);
	shader->setUniform("u_iRes", Vector2(1.0 / static_cast<float>(Application::instance->window_width),
										 1.0 / static_cast<float>(Application::instance->window_height)));

	shader->setUniform("u_viewprojection", camera->viewprojection_matrix);

	
	glEnable(GL_BLEND);
	glBlendFunc(GL_ONE, GL_ONE);
	glEnable(GL_CULL_FACE);
	

	for (const auto light: lights)
	{
		Matrix44 m;
		m.setTranslation(light->model.getTranslation().x, light->model.getTranslation().y, light->model.getTranslation().z);
		m.scale(light->max_distance, light->max_distance, light->max_distance);
		
		shader->setUniform("u_model", m);
		
		upload_light_to_shader(shader, light);
		
		glFrontFace(GL_CW);
		
		mesh->render(GL_TRIANGLES);
	}
	glFrontFace(GL_CCW);
	glDisable(GL_BLEND);
	glDisable(GL_CULL_FACE);
		
}

void Renderer::render_ambient_occlusion(Camera* camera, GTR::Scene* scene)
{

	glDisable(GL_DEPTH_TEST);
	glDisable(GL_BLEND);
	
	Mesh* quad = Mesh::getQuad();
	Shader* shader  = Shader::Get("ssao");
	shader->enable();

	shader->setUniform("u_gb1_texture", gbuffers_fbo->color_textures[1], 1);
	shader->setUniform("u_depth_texture", gbuffers_fbo->depth_texture, 3);

	Matrix44 inverse_view_projection = camera->viewprojection_matrix;
	inverse_view_projection.inverse();

	
	shader->setUniform("u_viewprojection", camera->viewprojection_matrix);
	shader->setUniform("u_inverse_viewprojection", inverse_view_projection);
	shader->setUniform("u_iRes", Vector2(1.0 / static_cast<float>(Application::instance->window_width),
										 1.0 / static_cast<float>(Application::instance->window_height)));

	shader->setUniform3Array("u_points", reinterpret_cast<float*>(&random_points[0]), num_points);

	
	quad->render(GL_TRIANGLES);

	
}

void Renderer::render_forward(Camera* camera, GTR::Scene* scene)
{

	glClearColor(scene->background_color.x, scene->background_color.y, scene->background_color.z, 1.0);

	// Clear the color and the depth buffer
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	checkGLErrors();
	
	for (const auto& rc : render_calls)
	{
		
		if (camera->testBoxInFrustum(rc.world_bounding.center, rc.world_bounding.halfsize))
			if(use_single_pass)
			{
				render_mesh_with_material_single_pass(rc.model, rc.mesh, rc.material, camera);
			}else
			{
				render_mesh_with_material_and_lighting(rc.model, rc.mesh, rc.material, camera);
			}
	}

	for (auto& probe : probes)
	{
		renderProbe(probe.pos, 2, probe.sh.coeffs[0].v);
	}

	if(debug_probes_texture && probes_texture)
		probes_texture->toViewport();
	
}


void Renderer::render_deferred(Camera* camera, GTR::Scene* scene)
{
	
	glClearColor(scene->background_color.x, scene->background_color.y, scene->background_color.z, 1.0);

	// Clear the color and the depth buffer
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	checkGLErrors();

	// Render gBuffer
	//create gbuffers in case they don't exists

	//create and FBO
	if(!gbuffers_fbo)
	{
		gbuffers_fbo = new FBO();

		//create 3 textures of 4 components
		gbuffers_fbo->create( 	Application::instance->window_width, Application::instance->window_height, 
		3, 			//three textures
		GL_RGBA, 		//four channels
		GL_UNSIGNED_BYTE, //1 byte
		true );		//add depth_texture
	}

	if(!illumination_fbo)
	{
		illumination_fbo = new FBO();

		//create 3 textures of 4 components
		illumination_fbo->create( Application::instance->window_width, Application::instance->window_height, 
		1,
		GL_RGB, 		
		GL_FLOAT, 
		true );
	}

	if(!ambient_occlusion_fbo)
	{
		ambient_occlusion_fbo = new FBO();
		
		ambient_occlusion_fbo->create( Application::instance->window_width, Application::instance->window_height, 
		1, 			
		GL_LUMINANCE, 		
		GL_UNSIGNED_BYTE, 
		false );
	}
	//render each object with Gbuffer

	gbuffers_fbo->bind();

	glClearColor(scene->background_color.x, scene->background_color.y, scene->background_color.z, 1.0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);


	std::vector<render_call> render_call_blend_material;
	
	for (const auto& rc : render_calls)
	{
		if (camera->testBoxInFrustum(rc.world_bounding.center, rc.world_bounding.halfsize))
		{
			render_mesh_with_material_to_gbuffer(rc.model, rc.mesh, rc.material, camera);
			if(rc.material->alpha_mode == BLEND)
			{
				render_call_blend_material.push_back(rc);
			}
		}
	}

	gbuffers_fbo->unbind();
	

	ambient_occlusion_fbo->bind();
	// glClearColor(scene->background_color.x, scene->background_color.y, scene->background_color.z, 1.0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	render_ambient_occlusion(camera, scene);
	
	ambient_occlusion_fbo->unbind();
	

	
	illumination_fbo->bind();

	// glClearColor(0, scene->background_color.y, scene->background_color.z, 1.0);
	// glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	if(render_to_full_screen_quad){
		render_gbuffers_with_ambient(camera, scene);
		render_gbuffers_with_illumination_quad(camera, scene);
	}
	else
	{
		render_gbuffers_with_ambient(camera, scene);
		render_gbuffers_with_illumination_geometry(camera, scene);
	}
	
	gbuffers_fbo->depth_texture->copyTo(NULL);

	//Render transparent objects with forward pipeline
	glEnable(GL_DEPTH_TEST);
	for (const auto &rc : render_call_blend_material)
	{
		render_mesh_with_material_and_lighting(rc.model,rc.mesh, rc.material,camera);
	}
	
	illumination_fbo->unbind();
	glDisable(GL_BLEND);

	if(debug_gbuffers)
	{
		glDisable(GL_DEPTH_TEST);
		glDisable(GL_BLEND);


		const int height = Application::instance->window_height;
		const int width = Application::instance->window_width;
		
		//set an area of the screen and render fullscreen quad
		glViewport(0, height*0.5, width * 0.5, height * 0.5);
		gbuffers_fbo->color_textures[0]->toViewport(); //colorbuffer
		
		glViewport(width*0.5, height*0.5, width * 0.5, height * 0.5);
		gbuffers_fbo->color_textures[1]->toViewport(); //normalbuffer

		glViewport(width*0.5, height*0, width * 0.5, height * 0.5);
		gbuffers_fbo->color_textures[2]->toViewport(); //metallic and roughness
		
		//for the depth remember to linearize when displaying it
		glViewport(0, 0, width * 0.5, height * 0.5);
		Shader* depth_shader = Shader::getDefaultShader("linear_depth");
		depth_shader->enable();
		Vector2 near_far = Vector2(camera->near_plane, camera->far_plane);
		depth_shader->setUniform("u_camera_nearfar", near_far);
		gbuffers_fbo->depth_texture->toViewport(depth_shader);
		
		//set the viewport back to full screen
		glViewport(0,0,width,height);
	}else if(debug_ssao)
	{
		glDisable(GL_DEPTH_TEST);
		glDisable(GL_BLEND);
		ambient_occlusion_fbo->color_textures[0]->toViewport();
	}
	else
	{
		Shader* shader = Shader::Get("gamma");
		shader->enable();
		shader->setUniform("u_scale", scale);
		shader->setUniform("u_average_lum", average_lum);
		shader->setUniform("u_lumwhite2", lum_white);
		illumination_fbo->color_textures[0]->toViewport(shader);
	}
	
}
