#include <stdio.h>
#include <stdlib.h>
#include <SFML/Graphics.hpp>

int window_width;
int window_height;
sf::RenderWindow *window;
sf::Vertex *vertices;
sf::Texture *wall_texture;
sf::Texture *enemy_texture;
sf::Texture *sprite_texture;

#define VERTEX_COUNT 2

void init_workspace(int w_width, int w_height, char *w_title,
	int w_framerate_limit, char *wall_texture_path, char *enemy_texture_path,
	char *sprite_texture_path)
{
	window_width = w_width;
	window_height = w_height;
	window = new sf::RenderWindow(sf::VideoMode(w_width, w_height), w_title);
	window->setFramerateLimit(w_framerate_limit);
	vertices = new sf::Vertex[window_width * VERTEX_COUNT];
	wall_texture = new sf::Texture();
	wall_texture->loadFromFile(wall_texture_path);
	enemy_texture = new sf::Texture();
	enemy_texture->loadFromFile(enemy_texture_path);
	sprite_texture = new sf::Texture();
	sprite_texture->loadFromFile(sprite_texture_path);

	free(w_title);
	free(wall_texture_path);
}

#define CEILING_COLOR 64
#define FLOOR_COLOR 96

void update_workspace()
{
	sf::Event event;
	while(window->pollEvent(event))
	{
		if(event.type == sf::Event::Closed)
			window->close();
	}

	window->display();

	//Ceiling
	sf::RectangleShape rectangle(sf::Vector2f(0, 0));
	rectangle.setFillColor(
		sf::Color(CEILING_COLOR, CEILING_COLOR, CEILING_COLOR));
	rectangle.setSize(sf::Vector2f(window_width, window_height / 2));
	window->draw(rectangle);

	//Floor
	rectangle.move(sf::Vector2f(0, window_height / 2));
	rectangle.setFillColor(sf::Color(FLOOR_COLOR, FLOOR_COLOR, FLOOR_COLOR));
	window->draw(rectangle);
}

#define TEXTURE_SIZE 64
#define MAX_COLOR 255

void draw_line(int chosen_line, int x, double height, int texture, int color)
{
	if(chosen_line < 0 || chosen_line >= window_width)
		return;

	//Position
	vertices[chosen_line * VERTEX_COUNT].position.x = x + 1;
	vertices[chosen_line * VERTEX_COUNT + 1].position.x = x + 1;
	vertices[chosen_line * VERTEX_COUNT].position.y =
		(int)((window_height - height) / 2);
	vertices[chosen_line * VERTEX_COUNT + 1].position.y =
		(int)((window_height + height) / 2);

	//Texture coords
	vertices[chosen_line * VERTEX_COUNT].texCoords.x = texture;
	vertices[chosen_line * VERTEX_COUNT].texCoords.y = 0;
	vertices[chosen_line * VERTEX_COUNT + 1].texCoords.x = texture;
	vertices[chosen_line * VERTEX_COUNT + 1].texCoords.y = TEXTURE_SIZE;

	//Color
	vertices[chosen_line * VERTEX_COUNT].color =
		sf::Color(color, color, color, MAX_COLOR);
	vertices[chosen_line * VERTEX_COUNT + 1].color =
		sf::Color(color, color, color, MAX_COLOR);
}

#define TEXTURE_TYPE_WALL 0
#define TEXTURE_TYPE_ENEMY 1
#define TEXTURE_TYPE_SPRITE 2

void push_draw_buffer(int lines_count, int texture_type)
{
	sf::Texture *chosen_texture = 0;
	switch(texture_type)
	{
		case TEXTURE_TYPE_WALL:
			chosen_texture = wall_texture;
			break;
		case TEXTURE_TYPE_ENEMY:
			chosen_texture = enemy_texture;
			break;
		case TEXTURE_TYPE_SPRITE:
			chosen_texture = sprite_texture;
			break;
	}

	window->draw(vertices, lines_count * VERTEX_COUNT, sf::Lines,
		sf::RenderStates(chosen_texture));
}

int get_window_width()
{
	return window_width;
}

int get_window_height()
{
	return window_height;
}
