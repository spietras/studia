#pragma once
#include "JSON/json.hpp"
#include "SFML/Graphics.hpp"
#include "../Entities/Entity.h"
#include "../Entities/Door.h"
#include "../Entities/Key.h"
#include <unordered_map>

using json = nlohmann::json;

class Resources
{
public:
	enum class direction { RIGHT, LEFT, UP, DOWN };

	static json rooms;
	static json playerData;
	static std::unordered_map<std::string, sf::Texture> textures;
	static std::unordered_map<std::string, sf::Font> fonts;

	static void load(); //Loads every resource from disk
	static void save();
	static std::vector<Entity> createEntities(int roomId);
	static std::vector<Door> createDoors(int roomId, std::vector<bool> openedDoors_);
	static std::vector<Key> createKeys(int roomId, std::vector<bool> openedDoors_);

	static int countRooms();
	static int getRoomId(const std::string& roomName);
	static int highestDoorId();
};
