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
bool Renderer::use_single_pass = false;

void Renderer::renderScene(GTR::Scene* scene, Camera* camera)
{
	lights.clear();
	render_calls.clear();
	//set the clear color (the background color)
	glClearColor(scene->background_color.x, scene->background_color.y, scene->background_color.z, 1.0);

	// Clear the color and the depth buffer
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	checkGLErrors();

	//render entities
	for (int i = 0; i < scene->entities.size(); ++i)
	{
		BaseEntity* ent = scene->entities[i];

		if (!ent->visible)
			continue;

		if (ent->entity_type == LIGHT)
		{
			lights.push_back(dynamic_cast<GTR::light_entity*>(ent));
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

	for (const auto& light : lights)
	{
		generate_shadow_map(light);
	}

	for (const auto& rc : render_calls)
	{
		
		if (camera->testBoxInFrustum(rc.world_bounding.center, rc.world_bounding.halfsize))
			if(use_single_pass)
			{
				render_mesh_with_material_single_pass(rc.model, rc.mesh, rc.material, camera);
			}else
			{
				renderMeshWithMaterial(rc.model, rc.mesh, rc.material, camera);
			}
	}

	//glViewport(0, 0, 256, 256);
	//show_shadowmap(lights[0]);
	//glViewport(0, 0, Application::instance->window_width, Application::instance->window_height);
}

void GTR::Renderer::show_shadowmap(light_entity* light)
{
	Shader* shader = Shader::getDefaultShader("depth");
	shader->enable();
	shader->setUniform("u_camera_nearfar", Vector2(light->camera->near_plane, light->camera->far_plane));
	lights[0]->shadowmap->toViewport(shader);
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

//renders a mesh given its transform and material
void Renderer::renderMeshWithMaterial(const Matrix44 model, Mesh* mesh, GTR::Material* material, Camera* camera)
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

	//this is used to say which is the alpha threshold to what we should not paint a pixel on the screen (to cut polygons according to texture alpha)
	shader->setUniform("u_alpha_cutoff", material->alpha_mode == GTR::eAlphaMode::MASK ? material->alpha_cutoff : 0);

	shader->setUniform("u_ambient_light", scene->ambient_light);

	glDepthFunc(GL_LEQUAL);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE);


	for (const auto& light : lights)
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


		//do the draw call that renders the mesh into the screen
		mesh->render(GL_TRIANGLES);
		glEnable(GL_BLEND);
		shader->setUniform("u_ambient_light", Vector3());
	}


	glDisable(GL_BLEND);
	glDepthFunc(GL_LESS);

	//disable shader
	shader->disable();

	//set the render state as it was before to avoid problems with future renders
	glDisable(GL_BLEND);
}

inline void Renderer::render_mesh_with_material_single_pass(const Matrix44& model, Mesh* mesh, Material* material,
	Camera* camera)
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

	constexpr int max_lights = 5;

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
		light_entity* light = lights[i];
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

void GTR::Renderer::generate_shadow_map(light_entity* light)
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
