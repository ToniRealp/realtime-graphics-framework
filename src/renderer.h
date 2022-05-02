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

		std::vector<light_entity*> lights;
		std::vector<render_call> render_calls;

		//renders several elements of the scene
		void renderScene(GTR::Scene* scene, Camera* camera);
	
		//to render a whole prefab (with all its nodes)
		void renderPrefab(const Matrix44& model, GTR::Prefab* prefab, Camera* camera);

		//to render one node from the prefab and its children
		void renderNode(const Matrix44& model, GTR::Node* node, Camera* camera);
		void createRenderCall(Matrix44 model, Mesh* mesh, GTR::Material* material, Camera* camera);

		//to render one mesh given its material and transformation matrix
		void renderMeshWithMaterial(const Matrix44 model, Mesh* mesh, GTR::Material* material, Camera* camera);
	};

	class render_call
	{
	public:
		Mesh* mesh{};
		Material* material{};
		Matrix44 model;
		float distance_to_camera{};

		render_call() = default;

		render_call(Mesh* const mesh, Material* const material, const Matrix44& model, const float distance_to_camera)
			: mesh(mesh),
			  material(material),
			  model(model),
			  distance_to_camera(distance_to_camera)
		{
		}
	};

	Texture* CubemapFromHDRE(const char* filename);

};