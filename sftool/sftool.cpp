#include <stdio.h>
#include <stdlib.h>
#include <SFML/Graphics.hpp>
#include <SFML/Audio.hpp>

int lines_count;
float delta_time;
int window_width;
int window_height;
int scale_factor;
int health_bar_size;
sf::RenderWindow *window;
sf::Vertex *vertices;
sf::Texture *wall_texture;
sf::Texture *enemy_texture;
sf::Texture *sprite_texture;
sf::Shader *shader;
sf::Font *font;
sf::Music *music;
sf::RenderTexture *raycasting_texture;
sf::Sprite *raycasting_scene;
sf::Clock *timer;

#define VERTEX_COUNT 2
#define INIT_HEALTH_BAR_SIZE 100

void init_workspace(int w_width, int w_height, char *w_title,
	int w_framerate_limit, int s_factor, char *wall_texture_path,
	char *enemy_texture_path, char *sprite_texture_path,
	char *shader_path, char *font_path, char *music_path)
{
	lines_count = w_width / s_factor;
	window_width = w_width;
	window_height = w_height;
	scale_factor = s_factor;
	health_bar_size = INIT_HEALTH_BAR_SIZE;
	window = new sf::RenderWindow(sf::VideoMode(w_width, w_height), w_title);
	window->setFramerateLimit(w_framerate_limit);
	vertices = new sf::Vertex[lines_count * VERTEX_COUNT];
	wall_texture = new sf::Texture();
	wall_texture->loadFromFile(wall_texture_path);
	enemy_texture = new sf::Texture();
	enemy_texture->loadFromFile(enemy_texture_path);
	sprite_texture = new sf::Texture();
	sprite_texture->loadFromFile(sprite_texture_path);
	shader = new sf::Shader();
	shader->loadFromFile(shader_path, sf::Shader::Fragment);
	font = new sf::Font();
	font->loadFromFile(font_path);
	music = new sf::Music();
	music->openFromFile(music_path);
	music->setLoop(true);
	music->play();
	raycasting_texture = new sf::RenderTexture();
	raycasting_texture->create(lines_count, w_height);
	raycasting_texture->display();
	raycasting_scene = new sf::Sprite();
	raycasting_scene->setTexture(raycasting_texture->getTexture());
	raycasting_scene->scale(scale_factor, 1);
	timer = new sf::Clock;

	free(w_title);
	free(wall_texture_path);
	free(enemy_texture_path);
	free(sprite_texture_path);
	free(shader_path);
	free(font_path);
	free(music_path);
}

#define CEILING_COLOR 64
#define FLOOR_COLOR 96
#define HEALTH_BAR_X 15
#define HEALTH_BAR_Y 15
#define HEALTH_BAR_KOEF_X 8
#define HEALTH_BAR_SIZE_Y 16

void update_workspace()
{
	sf::Event event;
	while(window->pollEvent(event))
	{
		if(event.type == sf::Event::Closed)
			exit(0);
		if(event.type == sf::Event::KeyPressed)
			if(event.key.code == sf::Keyboard::Escape)
				exit(0);
	}

	delta_time = timer->getElapsedTime().asSeconds();
	timer->restart();

	raycasting_texture->display();
	window->draw(*raycasting_scene);

	//HealthBar
	sf::RectangleShape health_shape(sf::Vector2f(0, 0));
	health_shape.setFillColor(sf::Color::Red);
	health_shape.setSize(sf::Vector2f(
		HEALTH_BAR_KOEF_X * health_bar_size / scale_factor,
		HEALTH_BAR_SIZE_Y));
	health_shape.move(sf::Vector2f(HEALTH_BAR_X / scale_factor,
		HEALTH_BAR_Y));
	window->draw(health_shape);

	window->display();

	//Ceiling
	sf::RectangleShape back_shape(sf::Vector2f(0, 0));
	back_shape.setFillColor(
		sf::Color(CEILING_COLOR, CEILING_COLOR, CEILING_COLOR));
	back_shape.setSize(sf::Vector2f(lines_count, window_height / 2));
	raycasting_texture->draw(back_shape);

	//Floor
	back_shape.move(sf::Vector2f(0, window_height / 2));
	back_shape.setFillColor(sf::Color(FLOOR_COLOR, FLOOR_COLOR, FLOOR_COLOR));
	raycasting_texture->draw(back_shape);
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
	
	(int)(window_height / 2 - int(height) / 2);
	vertices[chosen_line * VERTEX_COUNT + 1].position.y =
		(int)(window_height / 2 + int(height) / 2);

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

#define THICKNESS_KOEF 4

void draw_text(char *msg, int x, int y, int size)
{
	sf::Text text;
	text.setFont(*font);
	text.setString(msg);
	text.setPosition(x, y);
	text.setCharacterSize(size);
	text.setOutlineThickness(size / THICKNESS_KOEF);
	raycasting_texture->draw(text);
	free(msg);
}

#define TEXTURE_TYPE_WALL 0
#define TEXTURE_TYPE_ENEMY 1
#define TEXTURE_TYPE_SPRITE 2

void push_draw_buffer(int count, int texture_type, int color_constant)
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

	shader->setUniform("color_constant", color_constant);
	raycasting_texture->draw(vertices, count * VERTEX_COUNT, sf::Lines,
		sf::RenderStates(sf::BlendAlpha, sf::Transform::Identity, chosen_texture, shader));
}

int get_lines_count()
{
	return lines_count;
}

int get_window_width()
{
	return window_width;
}

int get_window_height()
{
	return window_height;
}

int get_scale_factor()
{
	return scale_factor;
}

float get_delta_time()
{
	return delta_time;
}

void set_health_bar_size(int size)
{
	health_bar_size = size;
}

#define KEY_W 0
#define KEY_A 1
#define KEY_S 2
#define KEY_D 3
#define KEY_LEFT_ARROW 4
#define KEY_RIGHT_ARROW 5
#define KEY_I 6
#define KEY_SPACE 7

int get_key_pressed(int key_code)
{
	switch(key_code)
	{
		case KEY_W:
			return sf::Keyboard::isKeyPressed(sf::Keyboard::W);
		case KEY_A:
			return sf::Keyboard::isKeyPressed(sf::Keyboard::A);
		case KEY_S:
			return sf::Keyboard::isKeyPressed(sf::Keyboard::S);
		case KEY_D:
			return sf::Keyboard::isKeyPressed(sf::Keyboard::D);
		case KEY_LEFT_ARROW:
			return sf::Keyboard::isKeyPressed(sf::Keyboard::Left);
		case KEY_RIGHT_ARROW:
			return sf::Keyboard::isKeyPressed(sf::Keyboard::Right);
		case KEY_I:
			return sf::Keyboard::isKeyPressed(sf::Keyboard::I);
		case KEY_SPACE:
			return sf::Keyboard::isKeyPressed(sf::Keyboard::Space);
	}
	return 0;
}
