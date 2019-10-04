#pragma once

/**
* @file
* @brief Room class
*/

/** @cond */
#include <string>
#include <vector>
#include <SFML/Graphics.hpp>
/** @endcond */
#include "../Entities/Entity.h"
#include "../Entities/Misc/Key.h"
#include "../Entities/Misc/Door.h"
#include "../Entities/Misc/Portal.h"
#include "Resources.h"
#include "../Entities/Obstacles/Obstacle.h"

class Room
{
	std::string roomName_;
	int layerId_;
	std::vector<Entity> blocks_;
	std::vector<Key> keys_;
	std::vector<Door> doors_;
	std::vector<Portal> portals_;
	std::vector<std::unique_ptr<Obstacle>> obstacles_;
	sf::Color backgroundColor_;
	sf::RectangleShape background_;
	sf::Vector2f size_; //size of room from 0,0 to bottom right corner of bottom right block

	std::vector<std::vector<sf::Vertex>> gradientEdges_{}; //effect of fading to black on edges od the room

	void addGradient();
public:
	Room()
		: layerId_(0) {}

	/**
	 * \brief Constructs a room with given name. It will look up every data in Resources
	 * \param roomName Name of the room
	 */
	explicit Room(const std::string& roomName);

	std::string getRoomName() const { return roomName_; }
	int getLayerId() const { return layerId_; }
	sf::Vector2f getSize() const { return size_; }
	const std::vector<Entity>& getEntities() const { return blocks_; }
	std::vector<Key>& getKeys() { return keys_; }
	std::vector<Door>& getDoors() { return doors_; }
	std::vector<Portal>& getPortals() { return portals_; }
	std::vector<std::unique_ptr<Obstacle>>& getObstacles() { return obstacles_; }
	std::vector<std::vector<sf::Vertex>> getGradientEdges() const { return gradientEdges_; }
	sf::Color getBackgroundColor() const { return backgroundColor_; }
	sf::RectangleShape getBackground() const { return background_; }

	void update() { for(auto& obstacle : obstacles_) obstacle->update(); }
};
