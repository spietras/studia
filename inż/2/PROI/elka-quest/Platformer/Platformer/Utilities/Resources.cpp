#include "Resources.h"
#include <fstream>

json Resources::rooms_;
json Resources::playerData_;
json Resources::map_;
std::unordered_map<std::string, sf::Texture> Resources::textures_;

void Resources::load()
{
	std::ifstream i;

	i.open("Data/JSON/rooms.json");
	i >> rooms_;
	i.close();
	i.clear();

	i.open("Data/JSON/player.json");
	i >> playerData_;
	i.close();
	i.clear();

	i.open("Data/JSON/map.json");
	i >> map_;
	i.close();
	i.clear();

	textures_["player"].loadFromFile("Data/Textures/player.png");
	textures_["block"].loadFromFile("Data/Textures/block.png");
}

std::vector<Entity> Resources::createEntities(int roomId)
{
	std::vector<Entity> entities;
	std::string roomName = "room" + std::to_string(roomId);

	for(auto& entity : rooms_.at(roomName).at("entities"))
	{
		sf::Vector2f position(entity.at("positionX").get<float>(), entity.at("positionY").get<float>());

		entities.push_back(Entity(textures_["block"], position));
	}

	return entities;
}

int Resources::getRoomCount()
{
	int i = 0;

	for(json::iterator it = rooms_.begin(); it != rooms_.end(); ++it)
	{
		if(it.key().find("room") != std::string::npos) i++;
	}

	return i;
}

int Resources::getStartingRoomId()
{
	std::string roomName = map_.at("startingRoom").get<std::string>();

	std::string num = roomName.substr(4);

	return std::stoi(num);
}
