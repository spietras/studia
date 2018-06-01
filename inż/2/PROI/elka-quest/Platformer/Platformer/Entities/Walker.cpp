#include "Walker.h"

/* Sebastian Pietras */
void Walker::onCollision(const Entity& colliding, const sf::Vector2f push)
{
	MobileEntity::onCollision(colliding, push);
	if(fabs(push.x) > fabs(push.y)) walkRight_ = !walkRight_;
}

/* Sebastian Pietras */
void Walker::update(const float deltaTime,
                    const sf::Vector2f playerPos,
                    const bool isPlayerVisible,
                    std::vector<Bullet>& bullets)
{
	if(!isActive) return;
	if(onGround_) run(walkRight_);

	MobileEntity::update(deltaTime, playerPos, isPlayerVisible, bullets);
}
