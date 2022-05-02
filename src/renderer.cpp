#include "renderer.h"

#include <algorithm>

#include "camera.h"
#include "shader.h"
#include "mesh.h"
#include "texture.h"
#include "prefab.h"
#include "material.h"
#include "utils.h"
#include "scene.h"
#include "extra/hdre.h"


using namespace GTR;

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
			if(pent->prefab)
				renderPrefab(ent->model, pent->prefab, camera);
		}
	}

	std::sort(render_calls.begin(), render_calls.end(), [](render_call lhs, render_call rhs)
		{
			if (lhs.material->alpha_mode == GTR::eAlphaMode::NO_ALPHA && rhs.material->alpha_mode == GTR::eAlphaMode::NO_ALPHA)
			{
				return lhs.distance_to_camera < rhs.distance_to_camera;
			}
			else if(lhs.material->alpha_mode == GTR::eAlphaMode::NO_ALPHA && rhs.material->alpha_mode == GTR::eAlphaMode::BLEND)
			{
				return true;
			}
			else if (lhs.material->alpha_mode == GTR::eAlphaMode::BLEND && rhs.material->alpha_mode == GTR::eAlphaMode::NO_ALPHA)
			{
				return false;
			}
			else
			{
				return lhs.distance_to_camera < rhs.distance_to_camera;
			}
		});

	for (const auto& rc : render_calls)
	{
		renderMeshWithMaterial(rc.model, rc.mesh, rc.material, camera);
	}
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
		BoundingBox world_bounding = transformBoundingBox(node_model,node->mesh->box);
		
		//if bounding box is inside the camera frustum then the object is probably visible
		if (camera->testBoxInFrustum(world_bounding.center, world_bounding.halfsize) )
		{
			//render node mesh
			createRenderCall( node_model, node->mesh, node->material, camera );
			//node->mesh->renderBounding(node_model, true);
		}
	}

	//iterate recursively with children
	for (int i = 0; i < node->children.size(); ++i)
		renderNode(prefab_model, node->children[i], camera);
}


void Renderer::createRenderCall(const Matrix44 model, Mesh* mesh, GTR::Material* material, Camera* camera)
{
	Vector3 node_position = model * Vector3();
	float distance = node_position.distance(camera->eye);
	render_calls.emplace_back(mesh, material, model, distance);
}

//renders a mesh given its transform and material
void Renderer::renderMeshWithMaterial(const Matrix44 model, Mesh* mesh, GTR::Material* material, Camera* camera)
{
	//in case there is nothing to do
	if (!mesh || !mesh->getNumVertices() || !material )
		return;
    assert(glGetError() == GL_NO_ERROR);

	//define locals to simplify coding
	Shader* shader = NULL;
	Texture* texture = NULL;
	GTR::Scene* scene = GTR::Scene::instance;

	int num_lights = lights.size();

	texture = material->color_texture.texture;
	//texture = material->emissive_texture;
	//texture = material->metallic_roughness_texture;
	//texture = material->normal_texture;
	//texture = material->occlusion_texture;
	if (texture == NULL)
		texture = Texture::getWhiteTexture(); //a 1x1 white texture

	if (material->alpha_mode == GTR::eAlphaMode::BLEND)
	{
		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	}
	else
		glDisable(GL_BLEND);

	//select if render both sides of the triangles
	if(material->two_sided)
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
	shader->setUniform("u_model", model );
	shader->setUniform("u_time", t );

	shader->setUniform("u_color", material->color);
	if(texture) shader->setUniform("u_texture", texture, 0);

	//this is used to say which is the alpha threshold to what we should not paint a pixel on the screen (to cut polygons according to texture alpha)
	shader->setUniform("u_alpha_cutoff", material->alpha_mode == GTR::eAlphaMode::MASK ? material->alpha_cutoff : 0);

	shader->setUniform("u_ambient_light",scene->ambient_light);

	glDepthFunc(GL_LEQUAL);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE);

	if (!num_lights)
	{
		shader->setUniform("u_light_color", Vector3());
		shader->setUniform("u_light_position", Vector3());


		mesh->render(GL_TRIANGLES);
	}else
	{
		for (const auto &light : lights)
		{
			shader->setUniform("u_light_type", light->light_type);

			shader->setUniform("u_light_color", light->color * light->intensity);
			shader->setUniform("u_light_position", light->model * Vector3());
			shader->setUniform("u_light_max_distance", light->max_distance);

			shader->setUniform("u_light_cone_angle", static_cast<float>(cos(light->cone_angle * DEG2RAD)));
			shader->setUniform("u_light_exp", light->cone_exp);
			shader->setUniform("u_light_direction", light->model.rotateVector(Vector3(0, 0, -1)));


			//do the draw call that renders the mesh into the screen
			mesh->render(GL_TRIANGLES);
			glEnable(GL_BLEND);
			shader->setUniform("u_ambient_light", Vector3());
		}	
	}

	
	glDisable(GL_BLEND);
	glDepthFunc(GL_LESS);

	//disable shader
	shader->disable();

	//set the render state as it was before to avoid problems with future renders
	glDisable(GL_BLEND);
}


Texture* GTR::CubemapFromHDRE(const char* filename)
{
	HDRE* hdre = HDRE::Get(filename);
	if (!hdre)
		return NULL;

	Texture* texture = new Texture();
	if (hdre->getFacesf(0))
	{
		texture->createCubemap(hdre->width, hdre->height, (Uint8**)hdre->getFacesf(0),
			hdre->header.numChannels == 3 ? GL_RGB : GL_RGBA, GL_FLOAT);
		for (int i = 1; i < hdre->levels; ++i)
			texture->uploadCubemap(texture->format, texture->type, false,
				(Uint8**)hdre->getFacesf(i), GL_RGBA32F, i);
	}
	else
		if (hdre->getFacesh(0))
		{
			texture->createCubemap(hdre->width, hdre->height, (Uint8**)hdre->getFacesh(0),
				hdre->header.numChannels == 3 ? GL_RGB : GL_RGBA, GL_HALF_FLOAT);
			for (int i = 1; i < hdre->levels; ++i)
				texture->uploadCubemap(texture->format, texture->type, false,
					(Uint8**)hdre->getFacesh(i), GL_RGBA16F, i);
		}
	return texture;
}