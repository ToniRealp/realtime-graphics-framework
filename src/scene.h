#ifndef SCENE_H
#define SCENE_H

#include "framework.h"
#include "camera.h"
#include <string>

#include "texture.h"

//forward declaration
class cJSON; 


//our namespace
namespace GTR {
	class BaseEntity;

	class Scene
	{
	public:
		static Scene* instance;

		Vector3 background_color;
		Vector3 ambient_light;
		Camera main_camera;

		Scene();

		std::string filename;
		std::vector<BaseEntity*> entities;

		void clear();
		void addEntity(BaseEntity* entity);

		bool load(const char* filename);
		BaseEntity* createEntity(std::string type);
	};

	enum eEntityType {
		NONE = 0,
		PREFAB = 1,
		LIGHT = 2,
		CAMERA = 3,
		REFLECTION_PROBE = 4,
		DECALL = 5
	};

	class Prefab;

	//represents one element of the scene (could be lights, prefabs, cameras, etc)
	class BaseEntity
	{
	public:
		Scene* scene;
		std::string name;
		eEntityType entity_type;
		Matrix44 model;
		bool visible;
		BaseEntity() { entity_type = NONE; visible = true; }
		virtual ~BaseEntity() {}
		virtual void renderInMenu();
		virtual void configure(cJSON* json) {}
	};

	//represents one prefab in the scene
	class PrefabEntity : public GTR::BaseEntity
	{
	public:
		std::string filename;
		Prefab* prefab;
		
		PrefabEntity();
		virtual void renderInMenu();
		virtual void configure(cJSON* json);
	};

	enum LightType
	{
		point, spot, directional 
	};

	class LightEntity : public BaseEntity
	{
	public:
		Vector3 color;
		float intensity;
		int angle;
		float max_distance;
		LightType light_type;
		bool cast_shadows;
		float cone_angle;
		float cone_exp;
		float shadow_bias;

		FBO* fbo;
		Texture* shadowmap;
		Camera* camera;

		LightEntity();

		void renderInMenu() override;
		void configure(cJSON* json) override;
	};

	class ReflectionProbeEntity : public GTR::BaseEntity
	{
	public:
		Texture* texture;
		
		ReflectionProbeEntity();
		virtual void renderInMenu();
		virtual void configure(cJSON* json);
	};

};

#endif