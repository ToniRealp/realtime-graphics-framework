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
		enum RenderPipeline { FORWARD, DEFERRED };
		
		static bool use_single_pass;
		static bool debug_gbuffers;
		static bool render_to_full_screen_quad;
		static bool debug_ssao;
		static RenderPipeline render_pipeline;

		std::vector<LightEntity*> lights;
		std::vector<render_call> render_calls;

		FBO* gbuffers_fbo;
		FBO* illumination_fbo;
		FBO* ambient_occlusion_fbo;

		const int num_points = 64;
		std::vector<Vector3> random_points;

		Renderer();

		void render_forward(Camera* camera, GTR::Scene* scene);
		void render_ambient_occlusion(Camera* camera, GTR::Scene* scene, FBO* gbuffers_fbo);
		void render_deferred(Camera* camera, GTR::Scene* scene);
		//renders several elements of the scene
		void renderScene(GTR::Scene* scene, Camera* camera);
		void show_shadowmap(LightEntity* light);

		//to render a whole prefab (with all its nodes)
		void renderPrefab(const Matrix44& model, GTR::Prefab* prefab, Camera* camera);

		//to render one node from the prefab and its children
		void renderNode(const Matrix44& model, GTR::Node* node, Camera* camera);
		void createRenderCall(const Matrix44 model, Mesh* mesh, GTR::Material* material, Camera* camera, BoundingBox world_bounding);
		void upload_light_to_shader(Shader* shader, const LightEntity* light);

		//to render one mesh given its material and transformation matrix
		void render_mesh_with_material_and_lighting(const Matrix44 model, Mesh* mesh, GTR::Material* material, Camera* camera);
		void render_mesh_with_material_to_gbuffer(const Matrix44 model, Mesh* mesh, GTR::Material* material, Camera* camera);
		void render_gbuffers_with_illumination_quad(Camera* camera, Scene* scene);
		void render_gbuffers_with_illumination_geometry(Camera* camera, Scene* scene);
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