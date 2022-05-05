#pragma once
#include "prefab.h"

//forward declarations
class Camera;

namespace GTR {
	class render_call;

	class Prefab;
	class Material;
	
	// This class is in charge of rendering anything in our system.
	// Separating the render from anything else makes the code cleaner
	class Renderer
	{

	public:
		static bool use_single_pass;

		std::vector<LightEntity*> lights;
		std::vector<render_call> render_calls;

		
		//renders several elements of the scene
		void renderScene(GTR::Scene* scene, Camera* camera);
		void show_shadowmap(LightEntity* light);

		//to render a whole prefab (with all its nodes)
		void renderPrefab(const Matrix44& model, GTR::Prefab* prefab, Camera* camera);

		//to render one node from the prefab and its children
		void renderNode(const Matrix44& model, GTR::Node* node, Camera* camera);
		void createRenderCall(const Matrix44 model, Mesh* mesh, GTR::Material* material, Camera* camera, BoundingBox world_bounding);

		//to render one mesh given its material and transformation matrix
		void renderMeshWithMaterial(const Matrix44 model, Mesh* mesh, GTR::Material* material, Camera* camera);
		void render_mesh_with_material_single_pass(const Matrix44& model, Mesh* mesh, Material* material, Camera* camera);
		void render_flat_mesh(Matrix44 model, Mesh* mesh, GTR::Material* material, Camera* camera);
		void generate_shadow_map(LightEntity* light);
	};

	class render_call
	{
	public:
		Mesh* mesh{};
		Material* material{};
		Matrix44 model;
		float distance_to_camera{};
		BoundingBox world_bounding;

		render_call(Mesh* const mesh, Material* const material, const Matrix44& model, const float distance_to_camera,
			const BoundingBox& world_bounding)
			: mesh(mesh),
			  material(material),
			  model(model),
			  distance_to_camera(distance_to_camera),
			  world_bounding(world_bounding)
		{
		}

		render_call() = default;

	};

	Texture* CubemapFromHDRE(const char* filename);

};