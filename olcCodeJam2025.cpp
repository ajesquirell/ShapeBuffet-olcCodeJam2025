#define OLC_PGE_APPLICATION
#include "olcUTIL_Geometry2D.h"
#include "olcPixelGameEngine.h"

#define OLC_PGEX_SPLASHSCREEN
#include "olcPGEX_SplashScreen.h"

#define OLC_PGEX_MINIAUDIO
#include "olcPGEX_MiniAudio.h"

#include <random>

constexpr int BUBBLE_TONGUE_SIZE = 10;

constexpr float MAGNET_REEL_REACH = 200.0;
constexpr float MAGNET_REEL_STRENGTH = 400.0;

constexpr int MAX_COMBO_MULTIPLIER = 5;
constexpr int MAX_TARGET_STREAK_MULTIPLIER = 3;
constexpr int COMBO_MULTIPLIER_WINDOW_TIME_SEC = 3;

constexpr int WAVE_LENGTH = 15;
constexpr int GAME_LENGTH = 90;
constexpr int N_WAVES = GAME_LENGTH / WAVE_LENGTH;
constexpr int N_TARGET_CATCHES_NEEDED = 3;

// constexpr int SHAPE_RING_MULTIPLIER = 2;

constexpr float ENEMY_SPAWN_INTERVAL = 5.0f;

typedef double T;
typedef olc::v_2d<T> olc_v2d;
typedef olc::utils::geom2d::ray<T> olc_ray;
typedef olc::utils::geom2d::line<T> olc_line;
typedef olc::utils::geom2d::circle<T> olc_circle;
typedef olc::utils::geom2d::rect<T> olc_rect;
typedef olc::utils::geom2d::triangle<T> olc_triangle;
typedef olc::utils::geom2d::polygon<T> olc_polygon;

class ReflectableSurface;

static const T Epsilon = 1.0E-10;
inline bool is_equal(const T &a, const T &b, const T &epsilon = Epsilon)
{
	return std::abs(a - b) < epsilon;
}
inline bool is_equal(const olc_v2d &a, const olc_v2d &b, const T &epsilon = Epsilon)
{
	return is_equal(a.x, b.x, epsilon) && is_equal(a.y, b.y, epsilon);
}

int rand_int(int min, int max)
{
	std::random_device rd;
	std::mt19937 rng(rd());
	std::uniform_int_distribution<int> uni(min, max);
	auto random = uni(rng);
	return random;
}

void FillRotatedRectDecal(olc::PixelGameEngine *pge,
						  const olc::vf2d &center,
						  const olc::vf2d &size,
						  float angle_radians,
						  olc::Pixel tint = olc::WHITE)
{
	olc::Sprite s(1, 1);
	olc::Decal d(&s);
	pge->DrawRotatedDecal(center, &d, angle_radians,
						  {0.5f, 0.5f}, // rotate around the decal center
						  size,			// scale the 1x1 pixel
						  tint);
}

/* ===== Base Object =====*/
#pragma region Base Object
struct Object
{
	virtual ~Object() = default;

	olc_v2d pos;
	olc_v2d direction;
	olc::Pixel color = olc::WHITE;

	virtual void draw(olc::PixelGameEngine *pge, float fElapsedTime) const = 0;

	virtual bool check_contains(olc_v2d point) const = 0;

	virtual bool check_overlaps(olc_line line) const = 0;
	virtual bool check_overlaps(olc_circle circle) const = 0;
	virtual bool check_overlaps(olc_rect rect) const = 0;
	virtual bool check_overlaps(olc_triangle tri) const = 0;

	virtual std::optional<olc_v2d> check_intersects(olc_line line) const = 0;
	virtual std::optional<olc_v2d> check_intersects(olc_circle circle) const = 0;
	virtual std::optional<olc_v2d> check_intersects(olc_rect rect) const = 0;
	virtual std::optional<olc_v2d> check_intersects(olc_triangle tri) const = 0;

	virtual std::unique_ptr<ReflectableSurface> get_reflectable_surface() const = 0;
};

/* ===== Shapes =====*/
#pragma region SHAPES

class ReflectableSurface
{
public:
	virtual ~ReflectableSurface() = default;
	virtual std::optional<olc_ray> reflect(olc_ray input_ray) = 0;
	virtual void draw(olc::PixelGameEngine *pge, olc::Pixel color = olc::WHITE) = 0;

	virtual std::optional<olc_v2d> intersects_with_object(const Object &object) = 0;

	std::optional<olc_v2d> reflect_object(const Object &object)
	{
		std::optional<olc_v2d> intersection = intersects_with_object(object);
		if (!intersection)
			return std::nullopt;

		std::optional<olc_ray> reflected_ray = reflect(olc_ray(*intersection, object.direction));
		if (!reflected_ray)
			return std::nullopt;

		return reflected_ray->direction;
	}

private:
	virtual bool overlaps_with_object(const Object &object) = 0;
};

class Line : public ReflectableSurface, public olc_line
{
public:
	Line() = default;
	Line(olc_v2d start, olc_v2d end) : olc_line(start, end) {}

	virtual std::optional<olc_ray> reflect(olc_ray input_ray) override
	{
		return olc::utils::geom2d::reflect(input_ray, *this);
	}

	virtual std::optional<olc_v2d> intersects_with_object(const Object &object) override
	{
		return object.check_intersects(*this);
	}

	virtual void draw(olc::PixelGameEngine *pge, olc::Pixel color = olc::WHITE) override
	{
		pge->DrawLine(start.x, start.y, end.x, end.y, color);
	}

private:
	virtual bool overlaps_with_object(const Object &object) override
	{
		return object.check_overlaps(*this);
	}
};

class Circle : public ReflectableSurface, public olc_circle
{
public:
	Circle() = default;
	Circle(olc_v2d center, T radius) : olc_circle(center, radius) {}

	virtual std::optional<olc_ray> reflect(olc_ray input_ray) override
	{
		return olc::utils::geom2d::reflect(input_ray, *this);
	}

	virtual std::optional<olc_v2d> intersects_with_object(const Object &object) override
	{
		return object.check_intersects(*this);
	}

	virtual void draw(olc::PixelGameEngine *pge, olc::Pixel color = olc::WHITE) override
	{
		pge->DrawCircle(pos.x, pos.y, radius, color);
	}

private:
	virtual bool overlaps_with_object(const Object &object) override
	{
		return object.check_overlaps(*this);
	}
};

class Rectangle : public ReflectableSurface, public olc_rect
{
public:
	Rectangle() = default;
	Rectangle(olc_v2d pos, olc_v2d size) : olc_rect(pos, size) {}

	virtual std::optional<olc_ray> reflect(olc_ray input_ray) override
	{
		return olc::utils::geom2d::reflect(input_ray, *this);
	}

	virtual std::optional<olc_v2d> intersects_with_object(const Object &object) override
	{
		return object.check_intersects(*this);
	}

	virtual void draw(olc::PixelGameEngine *pge, olc::Pixel color = olc::WHITE) override
	{
		pge->DrawRect(pos.x, pos.y, size.x, size.y, color);
	}

private:
	virtual bool overlaps_with_object(const Object &object) override
	{
		return object.check_overlaps(*this);
	}
};

class Triangle : public ReflectableSurface, public olc_triangle
{
public:
	Triangle() = default;
	Triangle(olc_v2d p1, olc_v2d p2, olc_v2d p3) : olc_triangle(p1, p2, p3) {}

	virtual std::optional<olc_ray> reflect(olc_ray input_ray) override
	{
		return olc::utils::geom2d::reflect(input_ray, *this);
	}

	virtual std::optional<olc_v2d> intersects_with_object(const Object &object) override
	{
		return object.check_intersects(*this);
	}

	virtual void draw(olc::PixelGameEngine *pge, olc::Pixel color = olc::WHITE) override
	{
		pge->DrawTriangle(pos[0].x, pos[0].y, pos[1].x, pos[1].y, pos[2].x, pos[2].y, color);
	}

private:
	virtual bool overlaps_with_object(const Object &object) override
	{
		return object.check_overlaps(*this);
	}
};

/* ===== Shapes END =====*/

#pragma region Derived Objects

struct CircleObject : virtual public Object
{
	CircleObject(olc_v2d position, T radius)
	{
		pos = position;
		this->radius = radius;
	}

	T radius;

	virtual void draw(olc::PixelGameEngine *pge, float fElapsedTime) const override
	{
		pge->FillCircle(pos, radius, color);
	}

	virtual bool check_contains(olc_v2d point) const override
	{
		return olc::utils::geom2d::contains(olc_circle(pos, radius), point);
	}

	virtual bool check_overlaps(olc_line line) const override
	{
		return olc::utils::geom2d::overlaps(olc_circle(pos, radius), line);
	}
	virtual bool check_overlaps(olc_circle circle) const override
	{
		return olc::utils::geom2d::overlaps(olc_circle(pos, radius), circle);
	}
	virtual bool check_overlaps(olc_rect rect) const override
	{
		return olc::utils::geom2d::overlaps(olc_circle(pos, radius), rect);
	}
	virtual bool check_overlaps(olc_triangle tri) const override
	{
		return olc::utils::geom2d::overlaps(olc_circle(pos, radius), tri);
	}

	virtual std::optional<olc_v2d> check_intersects(olc_line line) const override
	{
		std::vector<olc_v2d> intersections = olc::utils::geom2d::intersects(olc_circle(pos, radius), line);
		if (!intersections.empty())
		{
			return intersections[0];
		}
		return std::nullopt;
	}
	virtual std::optional<olc_v2d> check_intersects(olc_circle circle) const override
	{
		std::vector<olc_v2d> intersections = olc::utils::geom2d::intersects(olc_circle(pos, radius), circle);
		if (!intersections.empty())
		{
			return intersections[0];
		}
		return std::nullopt;
	}
	virtual std::optional<olc_v2d> check_intersects(olc_rect rect) const override
	{
		std::vector<olc_v2d> intersections = olc::utils::geom2d::intersects(olc_circle(pos, radius), rect);
		if (!intersections.empty())
		{
			return intersections[0];
		}
		return std::nullopt;
	}
	virtual std::optional<olc_v2d> check_intersects(olc_triangle tri) const override
	{
		std::vector<olc_v2d> intersections = olc::utils::geom2d::intersects(olc_circle(pos, radius), tri);
		if (!intersections.empty())
		{
			return intersections[0];
		}
		return std::nullopt;
	}

	virtual std::unique_ptr<ReflectableSurface> get_reflectable_surface() const override { return std::make_unique<Circle>(pos, radius); }
};

// TODO maybe allow these to be rotatable. For now just going to have them move in axis-aligned directions for collision simplicity.
struct RectangleObject : virtual public Object
{
	RectangleObject(olc_v2d position, T size)
	{
		pos = position; // Middle point
		this->size = olc_v2d(size, size);
	}

	olc_v2d size;

	virtual void draw(olc::PixelGameEngine *pge, float fElapsedTime) const override
	{
		pge->FillRect(pos - size / 2, size, color);
	}

	virtual bool check_contains(olc_v2d point) const override
	{
		return olc::utils::geom2d::contains(rectangle(), point);
	}

	virtual bool check_overlaps(olc_line line) const override
	{
		return olc::utils::geom2d::overlaps(rectangle(), line);
	}
	virtual bool check_overlaps(olc_circle circle) const override
	{
		return olc::utils::geom2d::overlaps(rectangle(), circle);
	}
	virtual bool check_overlaps(olc_rect rect) const override
	{
		return olc::utils::geom2d::overlaps(rectangle(), rect);
	}
	virtual bool check_overlaps(olc_triangle tri) const override
	{
		return olc::utils::geom2d::overlaps(rectangle(), tri);
	}

	virtual std::optional<olc_v2d> check_intersects(olc_line line) const override
	{
		std::vector<olc_v2d> intersections = olc::utils::geom2d::intersects(rectangle(), line);
		if (!intersections.empty())
		{
			return intersections[0];
		}
		return std::nullopt;
	}
	virtual std::optional<olc_v2d> check_intersects(olc_circle circle) const override
	{
		std::vector<olc_v2d> intersections = olc::utils::geom2d::intersects(rectangle(), circle);
		if (!intersections.empty())
		{
			return intersections[0];
		}
		return std::nullopt;
	}
	virtual std::optional<olc_v2d> check_intersects(olc_rect rect) const override
	{
		std::vector<olc_v2d> intersections = olc::utils::geom2d::intersects(rectangle(), rect);
		if (!intersections.empty())
		{
			return intersections[0];
		}
		return std::nullopt;
	}
	virtual std::optional<olc_v2d> check_intersects(olc_triangle tri) const override
	{
		std::vector<olc_v2d> intersections = olc::utils::geom2d::intersects(rectangle(), tri);
		if (!intersections.empty())
		{
			return intersections[0];
		}
		return std::nullopt;
	}

	virtual std::unique_ptr<ReflectableSurface> get_reflectable_surface() const override { return std::make_unique<Rectangle>(pos, size); }

private:
	olc_rect rectangle() const
	{
		return olc_rect(pos - size / 2, size);
	}
};

struct TriangleObject : virtual public Object
{
	TriangleObject(olc_v2d position, T size)
	{
		pos = position;
		this->size = size;
	}

	T size;

	virtual void draw(olc::PixelGameEngine *pge, float fElapsedTime) const override
	{
		olc_triangle t = triangle();
		pge->FillTriangle(t.pos[0], t.pos[1], t.pos[2], color);
	}

	virtual bool check_contains(olc_v2d point) const override
	{
		return olc::utils::geom2d::contains(triangle(), point);
	}

	virtual bool check_overlaps(olc_line line) const override
	{
		return olc::utils::geom2d::overlaps(triangle(), line);
	}
	virtual bool check_overlaps(olc_circle circle) const override
	{
		return olc::utils::geom2d::overlaps(triangle(), circle);
	}
	virtual bool check_overlaps(olc_rect rect) const override
	{
		return olc::utils::geom2d::overlaps(triangle(), rect);
	}
	virtual bool check_overlaps(olc_triangle tri) const override
	{
		return olc::utils::geom2d::overlaps(triangle(), tri);
	}

	virtual std::optional<olc_v2d> check_intersects(olc_line line) const override
	{
		std::vector<olc_v2d> intersections = olc::utils::geom2d::intersects(triangle(), line);
		if (!intersections.empty())
		{
			return intersections[0];
		}
		return std::nullopt;
	}
	virtual std::optional<olc_v2d> check_intersects(olc_circle circle) const override
	{
		std::vector<olc_v2d> intersections = olc::utils::geom2d::intersects(triangle(), circle);
		if (!intersections.empty())
		{
			return intersections[0];
		}
		return std::nullopt;
	}
	virtual std::optional<olc_v2d> check_intersects(olc_rect rect) const override
	{
		std::vector<olc_v2d> intersections = olc::utils::geom2d::intersects(triangle(), rect);
		if (!intersections.empty())
		{
			return intersections[0];
		}
		return std::nullopt;
	}
	virtual std::optional<olc_v2d> check_intersects(olc_triangle tri) const override
	{
		std::vector<olc_v2d> intersections = olc::utils::geom2d::intersects(triangle(), tri);
		if (!intersections.empty())
		{
			return intersections[0];
		}
		return std::nullopt;
	}

	virtual std::unique_ptr<ReflectableSurface> get_reflectable_surface() const override
	{
		olc_triangle t = triangle();
		return std::make_unique<Triangle>(t.pos[0], t.pos[1], t.pos[2]);
	}

protected:
	olc_triangle triangle() const
	{
		olc_v2d norm = direction.norm();
		olc_v2d center_point = pos - norm * size / 2;
		olc_v2d perp_vector = norm.perp() * size / 2;
		return olc_triangle(pos + norm * size / 2, center_point + perp_vector, center_point - perp_vector);
	}
};

/* ===== Player =====*/
#pragma region PLAYER
struct Player : public CircleObject
{
	Player() : Player({0.0, 0.0}) {}
	Player(olc_v2d pos) : CircleObject(pos, 6)
	{
		color = olc::GREEN;
	}

	olc_v2d tongue_direction;

	T max_helper_distance = 60;
	T max_tongue_distance = 500;

	T current_tongue_distance = 0;

	T tongue_out_speed = 600.0;
	T tongue_in_speed = 750.0;
	T movement_speed = 100.0;

	float knockback_time_counter = 0;
	T knockback_speed = 0;
	olc_v2d knockback_direction;
	bool is_stunned = false;

	struct
	{
		bool active = false;
		bool engaging = false;
		bool retracting = false;
	} tongue;

	struct
	{
		bool bubble_tongue = false;
		bool piercing_tip = false;
		bool magnet_reel = false;
	} abilities;

	bool is_hit() { return knockback_time_counter > 0; }
};

/* ===== Player END =====*/

/* ===== Enemy Objects =====*/
#pragma region ENEMIES

enum ShapeId
{
	NONE,
	CIRCLE,
	RECTANGLE,
	TRIANGLE
};
ShapeId random_shape_id() { return static_cast<ShapeId>(rand_int(ShapeId::CIRCLE, ShapeId::TRIANGLE)); }

struct Enemy : virtual public Object
{
	// Enemy() {
	// 	expire_time = rand_int(8, 12);
	// }
	virtual ~Enemy() = default;
	virtual void behavior(float fElapsedTime, const Player &player) = 0;
	virtual ShapeId get_shape_id() = 0;

	virtual void on_collision() {}

	virtual void on_tongue_hit(int n_tongue_bounces)
	{
		this->caught = true;
		this->n_tongue_bounces_when_caught = n_tongue_bounces;
	}

	void hit_player(Player &player)
	{
		player.knockback_time_counter = stun_amount;
		player.knockback_direction = olc_v2d(player.pos - pos).norm();
		player.knockback_speed = movement_speed;
		player.is_stunned = this->stun_player_on_hit;
		this->on_hit_player(player);
	}

	virtual int points_lost_on_hit() { return 0; }

	bool is_caught() const { return this->caught; }

	float movement_speed = 100.0;

	int points_worth = 1;
	int n_tongue_bounces_when_caught = 0;

	T piercing_tip_caught_distance;

	// float expire_time = 0.0;

protected:
	float stun_amount = 0.5;
	bool stun_player_on_hit = false;

	bool caught = false;

	virtual void on_hit_player(Player &player) {}
};

struct CircleFly : public Enemy, public CircleObject
{
	CircleFly(olc_v2d position) : CircleFly(position, 20) {}

	virtual ShapeId get_shape_id() override { return ShapeId::CIRCLE; }

	virtual void behavior(float fElapsedTime, const Player &player) override
	{
		UNUSED(player);
		time_counter += fElapsedTime;
		if (time_counter >= timeout)
		{
			randomize_direction();
		}
	}

	virtual void draw(olc::PixelGameEngine *pge, float fElapsedTime) const override
	{
		CircleObject::draw(pge, fElapsedTime);

		pge->FillCircle(pos, radius * 0.9, olc::PixelLerp(color, olc::WHITE, 0.2f));
		pge->FillCircle(pos, radius * 0.8, olc::PixelLerp(color, olc::WHITE, 0.4f));
		pge->FillCircle(pos, radius * 0.7, olc::PixelLerp(color, olc::WHITE, 0.6f));
	}

protected:
	CircleFly(olc_v2d position, T radius) : CircleObject(position, radius)
	{
		randomize_direction();
		color = olc::GREY;
	}

private:
	void randomize_direction()
	{
		T random_angle = rand_int(0, 360) * M_PI / 180.0;
		direction = olc_v2d(std::cos(random_angle), std::sin(random_angle)).norm();

		timeout = rand_int(4, 12);
		time_counter = 0;
	}

	float time_counter;
	float timeout;
};

struct SquareBeetle : public Enemy, public RectangleObject
{
	SquareBeetle(olc_v2d position) : RectangleObject(position, 28)
	{
		randomize_direction();
		color = olc::GREY;
	}

	virtual ShapeId get_shape_id() override { return ShapeId::RECTANGLE; }

	virtual void behavior(float fElapsedTime, const Player &player) override
	{
		UNUSED(player);
		time_counter += fElapsedTime;
		if (time_counter >= timeout)
		{
			randomize_direction();
		}

		if (!has_armor)
		{
			armor_time_counter += fElapsedTime;
			if (armor_time_counter > 3)
			{
				has_armor = true;
			}
		}

		movement_speed = has_armor ? 50.0 : 220.0;
	}

	virtual void on_collision() override
	{
		// Kind of a hack, but override the reflected direction to give it a different personality
		// Like it can't be knocked around
		randomize_direction();
	}

	virtual void on_tongue_hit(int n_tongue_bounces) override
	{
		if (has_armor)
		{
			has_armor = false;
			armor_time_counter = 0;
		}
		else
		{
			Enemy::on_tongue_hit(n_tongue_bounces);
		}
	}

	virtual void draw(olc::PixelGameEngine *pge, float fElapsedTime) const override
	{
		RectangleObject::draw(pge, fElapsedTime);
		if (has_armor)
		{
			// Draw shield icon
			static olc::Pixel shield_color = olc::PixelLerp(olc::DARK_BLUE, olc::CYAN, 0.5f);
			pge->FillRectDecal(pos - size / 4, olc_v2d(size.x / 2, size.y / 4), shield_color);
			pge->FillTriangleDecal(pos + olc_v2d(0, size.y / 3), pos + olc_v2d(size.x / 4, 0), pos + olc_v2d(-size.x / 4, 0), shield_color);
			pge->FillTriangleDecal(pos + olc_v2d(0, -size.y / 3.2), pos + olc_v2d(size.x / 4, -size.y / 4), pos + olc_v2d(-size.x / 4, -size.y / 4), shield_color);
		}
	}

private:
	void randomize_direction()
	{
		int random_dir = rand_int(0, 3);
		T x, y;
		if (random_dir == 0)
		{
			x = 0;
			y = -1;
		}
		else if (random_dir == 1)
		{
			x = 1;
			y = 0;
		}
		else if (random_dir == 2)
		{
			x = 0;
			y = 1;
		}
		else if (random_dir == 3)
		{
			x = -1;
			y = 0;
		}
		direction = olc_v2d(x, y).norm();

		timeout = rand_int(2, 4);
		time_counter = 0;
	}

	float time_counter;
	float timeout;

	bool has_armor = true;
	float armor_time_counter = 0;
};

struct TriangleWasp : public Enemy, public TriangleObject
{
	TriangleWasp(olc_v2d position) : TriangleObject(position, 20)
	{
		randomize_direction();
		init_timer(4); // Give player a change to not get charged at the beginning
		color = olc::YELLOW;

		stun_player_on_hit = true;
		stun_amount = 2;
	}

	virtual ShapeId get_shape_id() override { return ShapeId::TRIANGLE; }

	virtual void behavior(float fElapsedTime, const Player &player) override
	{
		time_counter += fElapsedTime;
		if (time_counter >= timeout)
		{
			bool decide_to_charge = rand_int(0, 2) == 0;
			if (!charge_player && decide_to_charge)
			{
				charge_player = true;
				init_timer(1);
			}
			else
			{
				charge_player = false;
				randomize_direction();
				init_timer();
			}
		}

		movement_speed = charge_player ? 150.0 : 90.0;

		if (charge_player)
		{
			direction = (player.pos - pos).norm();
		}
	}

	virtual void on_collision() override
	{
		charge_player = false;
	}

	// TODO penalty if hit and not charging?
	// virtual void on_tongue_hit() override
	// {
	// }

	virtual void on_hit_player(Player &player) override
	{
		charge_player = false;
	}

	virtual int points_lost_on_hit() override
	{
		return 3;
	}

	virtual void draw(olc::PixelGameEngine *pge, float fElapsedTime) const override
	{
		if (charge_player)
		{
			static float animation_timer = 0;
			static bool animation_dir = false;
			static float t = 0;
			animation_timer -= fElapsedTime;
			if (animation_timer <= 0)
			{
				animation_timer = 0.01;
				t = animation_dir ? t + 0.1 : t - 0.1;
				if (t < 0 || t > 1)
				{
					animation_dir = !animation_dir;
					t = std::clamp(t, 0.0f, 1.0f);
				}
			}

			olc_triangle tri = triangle();
			pge->FillTriangle(tri.pos[0], tri.pos[1], tri.pos[2], olc::PixelLerp(olc::RED, olc::YELLOW, t));
		}
		else
		{
			TriangleObject::draw(pge, fElapsedTime);
		}
	}

private:
	void randomize_direction()
	{
		T random_angle = rand_int(0, 360) * M_PI / 180.0;
		direction = olc_v2d(std::cos(random_angle), std::sin(random_angle)).norm();
	}
	void init_timer(int timeout = -1)
	{
		this->timeout = timeout == -1 ? rand_int(2, 3) : timeout;
		this->time_counter = 0;
	}

	float time_counter;
	float timeout;

	bool charge_player = false;
};

/* ===== Enemy Shapes END =====*/

/* ===== Scene =====*/
#pragma region SCENE
class Scene
{
public:
	void add_object(std::unique_ptr<ReflectableSurface> r)
	{
		shapes.push_back(std::move(r));
	}

	inline bool closest_reflection(const olc_ray &start_ray, olc_ray &closest_reflected_ray)
	{
		T closest_distance;
		return closest_reflection(start_ray, closest_reflected_ray, closest_distance);
	}
	inline bool closest_reflection(const olc_ray &start_ray, olc_ray &closest_reflected_ray, T &closest_distance)
	{
		bool hit = false;
		closest_distance = std::numeric_limits<T>::max();

		for (size_t i = 0; i < shapes.size(); i++)
		{
			const std::unique_ptr<ReflectableSurface> &shape = shapes[i];
			std::optional<olc_ray> reflected_ray = shape->reflect(start_ray);
			if (!reflected_ray)
				continue;

			T distance = (start_ray.origin - reflected_ray->origin).mag();

			// Ensure we do not hit the same shape again at the same point
			if (distance < closest_distance &&
				!is_equal(reflected_ray->origin, start_ray.origin) &&
				!is_equal(reflected_ray->direction, start_ray.direction))
			{
				closest_distance = distance;
				closest_reflected_ray = *reflected_ray;
				hit = true;
			}
		}
		return hit;
	}

	std::vector<olc_v2d> trace(olc_ray start_ray, T max_path_distance)
	{
		if (max_path_distance <= 0)
			return {};

		std::vector<olc_v2d> ray_path;
		// ray_path.reserve(settings.max_bounces);
		ray_path.push_back(start_ray.origin);

		T total_distance = 0;

		// for (int bounce = 0; bounce < settings.max_bounces; bounce++)
		while (true)
		{
			olc_ray closest_reflected_ray;
			T closest_distance;
			bool hit = closest_reflection(start_ray, closest_reflected_ray, closest_distance);

			if (!hit)
			{
				// Ray didn't hit anything - Add remaining distance to ray path
				T last_segment_distance = max_path_distance - total_distance;
				olc_v2d unit = start_ray.direction.norm();
				ray_path.push_back(start_ray.origin + unit * last_segment_distance);
				break;
			}

			if (total_distance + closest_distance <= max_path_distance)
			{
				ray_path.push_back(closest_reflected_ray.origin);
			}
			else
			{
				// Finish out the remaining distance
				T last_segment_distance = max_path_distance - total_distance;
				olc_v2d unit = start_ray.direction.norm();
				ray_path.push_back(start_ray.origin + unit * last_segment_distance);
				break; // Max path distance reached
			}

			start_ray = closest_reflected_ray;
			total_distance += closest_distance;

			// We don't have control over the geometry math here to ensure the reflect/collision functions do not include collisions that are on the ray origin.
			// (although we could modify the olcUTIL_Geometry2D header)
			// So we need to "nudge" the ray slightly to avoid collisions at the same spot.
			start_ray.origin += start_ray.direction * Epsilon;
		}

		return ray_path;
	}

	std::optional<olc_v2d> reflect_object(const Object &object)
	{
		for (const auto &shape : shapes)
		{
			std::optional<olc_v2d> reflected_direction = shape->reflect_object(object);
			if (reflected_direction)
				return reflected_direction;
		}
		return std::nullopt;
	}

	// TODO each one needs a color though.....
	void draw(olc::PixelGameEngine *pge)
	{
		for (const auto &shape : shapes)
		{
			shape->draw(pge);
		}
	}

private:
	std::vector<std::unique_ptr<ReflectableSurface>> shapes;
};

/* ===== Scene END =====*/

struct GameState
{
	int score = 0;

	struct
	{
		int multiplier = 0;
		float window_time = 0.0;
	} current_combo_streak;

	struct
	{
		int multiplier = 0;
	} current_target_streak;

	int wave_number = 0;
	float wave_timer = 0;

	ShapeId current_target_shape = ShapeId::NONE;
	int current_target_shape_streak = 0; // Number of target bugs caught so far this wave

	// bool shape_ring_active = false;
	// bool shape_ring_multiplier_active = false;

	struct
	{
		int enemies_caught = 0;
		int highest_multiplier = 0;
		int highest_shape_streak = 0;
		int highest_tongue_bounces_catch = 0;
	} stats;
};

#pragma region ENGINE
class OlcCodeJam2025 : public olc::PixelGameEngine
{
public:
	OlcCodeJam2025()
	{
		sAppName = "Shape Buffet! - olcCodeJam2025"; // TODO NAME
	}

	olc::SplashScreen s; // Automatically hooks into PGE just by existing

	olc::MiniAudio ma;
	int audio_id;
	bool playing_music = false;

	olc::Sprite title;
	std::unique_ptr<olc::Decal> title_decal;

	bool on_title_screen = true;
	bool on_help_screen = true;
	bool game_started = false;

	Scene scene;
	olc_rect scene_bounds;
	olc::vi2d score_location;

	Player player;

	bool tongue_debounce = false;

	std::vector<std::unique_ptr<Enemy>> enemies;

	GameState game_state;

	bool game_over = false;

	float new_points_timer = 0;
	int new_points_to_show = 0;

	float enemy_spawn_timer = 0;

public:
#pragma region OnUserCreate
	bool OnUserCreate() override
	{
		T W = ScreenWidth();
		T H = ScreenHeight();

		olc_v2d bounds_start(20, 60);
		olc_v2d bounds_end(W - 20, H - 20);
		scene_bounds = olc_rect(bounds_start, bounds_end - bounds_start);
		scene.add_object(std::make_unique<Rectangle>(scene_bounds.pos, scene_bounds.size));

		olc_v2d scene_center = scene_bounds.middle();

		init_player();

		score_location = olc::vi2d(scene_bounds.pos.x + scene_bounds.size.x - 150, 20);

		title = olc::Sprite("assets/shape_buffet.png");
		title_decal = std::make_unique<olc::Decal>(&title);

		audio_id = ma.LoadSound("assets/audio.mp3");

		return true;
	}

#pragma region OnUserUpdate
	bool OnUserUpdate(float fElapsedTime) override
	{
		if (!playing_music)
		{
			ma.Play(audio_id, true);
			playing_music = true;
		}

		if (on_title_screen)
		{
			DrawDecal(olc::vi2d(0, 0), title_decal.get(), olc::vf2d((float)ScreenWidth() / title.width, (float)ScreenHeight() / title.height));
			if (GetMouse(olc::Mouse::LEFT).bPressed)
			{
				on_title_screen = false;
			}
			return true;
		}

		if (GetKey(olc::Key::F1).bPressed)
		{
			on_help_screen = !on_help_screen;
		}

		if (on_help_screen)
		{
			SetPixelMode(olc::Pixel::ALPHA);
			FillRect(0, 0, ScreenWidth(), ScreenHeight(), olc::Pixel(0, 0, 0, 180));
			SetPixelMode(olc::Pixel::NORMAL);

			std::string help_text = "Move with WASD, aim with mouse, click to shoot tongue.\n\n"
									"Catch bugs to score points before time runs out!\n\n"
									"Full game is 90s spread over 6 waves.\n\n"
									"Every wave has a Target Shape - catch 3 to unlock ability!\n\n"
									"\t = Bubble Tongue - Larger tongue for easier catches.\n"
									"\t = Magnet Reel - Reels in bugs towards the tongue tip.\n"
									"\t = Piercing Tip - Spears multiple bugs in a single catch.\n\n"
									"Bug Types:\n\n"
									"\t = Fly: safe and common, only knocks you back.\n"
									"\t = Beetle: armored, needs two tongue hits.\n"
									"\t = Wasp: dangerous! Hit = -3 pts + 2s stun.\n\n"
									"Chain catches within 3s for up to x5 combo multiplier.\n\n"
									"Catch same bug type in a row for up to x3 streak multiplier.\n\n"
									"Bank shots off walls: each bounce adds bonus points!\n\n";

			int centerX = ScreenWidth() / 2;

			DrawStringPropDecal(olc_v2d(centerX - 200, 80), "How to Play", olc::RED, {6.0f, 6.0f});
			DrawStringPropDecal(olc_v2d(100, 160), help_text, olc::WHITE, {2.5f, 2.5f});

			int shape_y = 328;
			FillCircle(olc_v2d(164, shape_y), 6, olc::WHITE);
			FillRect(olc_v2d(158, shape_y + 16), olc_v2d(12, 12), olc::WHITE);
			FillTriangle(olc_v2d(164, shape_y + 38), olc_v2d(158, shape_y + 48), olc_v2d(170, shape_y + 48), olc::WHITE);

			int shape_y_2 = 448;
			FillCircle(olc_v2d(164, shape_y_2), 6, olc::WHITE);
			FillRect(olc_v2d(158, shape_y_2 + 16), olc_v2d(12, 12), olc::WHITE);
			FillTriangle(olc_v2d(164, shape_y_2 + 38), olc_v2d(158, shape_y_2 + 48), olc_v2d(170, shape_y_2 + 48), olc::WHITE);

			std::string bottom_text = game_started ? "Press F1 to return to game" : "Click anywhere to start!";
			DrawStringPropDecal(olc::vi2d(centerX - 160, ScreenHeight() - 80), bottom_text, olc::GREEN, {2.0f, 2.0f});

			if (!game_started && GetMouse(olc::Mouse::LEFT).bPressed)
			{
				game_started = true;
				on_help_screen = false;
			}

			return true;
		}

		Clear(olc::VERY_DARK_BLUE);

// === USER INPUT ===
#pragma region USER INPUT

		if (!game_over)
		{
			if (!player.is_hit())
			{
				if (GetKey(olc::Key::W).bHeld)
				{
					player.pos.y -= player.movement_speed * fElapsedTime;
				}
				if (GetKey(olc::Key::S).bHeld)
				{
					player.pos.y += player.movement_speed * fElapsedTime;
				}
				if (GetKey(olc::Key::A).bHeld)
				{
					player.pos.x -= player.movement_speed * fElapsedTime;
				}
				if (GetKey(olc::Key::D).bHeld)
				{
					player.pos.x += player.movement_speed * fElapsedTime;
				}

				player.tongue_direction = (GetMousePos() - player.pos).norm();
			}
			else
			{
				// Spin if stunned
				if (player.is_stunned)
				{
					T x = player.tongue_direction.x;
					T y = player.tongue_direction.y;
					T speed = 8.0;
					player.tongue_direction.x = x * std::cos(speed * fElapsedTime) - y * std::sin(speed * fElapsedTime);
					player.tongue_direction.y = x * std::sin(speed * fElapsedTime) + y * std::cos(speed * fElapsedTime);
				}

				// Move
				player.pos += player.knockback_direction * player.knockback_speed * fElapsedTime;

				player.knockback_time_counter -= fElapsedTime;
				if (player.knockback_time_counter <= 0)
				{
					player.knockback_time_counter = 0;
					player.is_stunned = false;
				}
			}

			player.pos.x = std::clamp(player.pos.x, scene_bounds.pos.x + player.radius, scene_bounds.pos.x + scene_bounds.size.x - player.radius);
			player.pos.y = std::clamp(player.pos.y, scene_bounds.pos.y + player.radius, scene_bounds.pos.y + scene_bounds.size.y - player.radius);

			// Engage tongue
			if (GetKey(olc::Key::SPACE).bPressed || GetMouse(olc::Mouse::LEFT).bPressed)
			{
				if (!player.tongue.active && !player.is_stunned)
				{
					player.tongue.active = true;
					tongue_debounce = false;
				}
			}
		}
		else
		{
			if (GetKey(olc::Key::SPACE).bReleased)
			{
				reset_game();
			}
		}

		if (player.tongue.active)
		{
			if (!player.tongue.engaging && !player.tongue.retracting)
			{
				player.tongue.engaging = true;
				player.tongue.retracting = false;
			}

			if (player.tongue.engaging)
			{
				player.current_tongue_distance += player.tongue_out_speed * fElapsedTime;
				if (player.current_tongue_distance >= player.max_tongue_distance)
				{
					player.tongue.retracting = true;
					player.tongue.engaging = false;
				}
			}
			else if (player.tongue.retracting)
			{
				player.current_tongue_distance -= player.tongue_in_speed * fElapsedTime;
				if (player.current_tongue_distance <= 0.0)
				{
					player.tongue.active = false;
					player.tongue.engaging = false;
					player.tongue.retracting = false;
				}
			}
		}
		player.current_tongue_distance = std::clamp(player.current_tongue_distance, 0.0, player.max_tongue_distance);

/* === Update Objects in Scene === */
#pragma region UPDATE OBJECTS

		olc_ray player_ray = olc_ray(player.pos, player.tongue_direction);

		// Ray trace helper path
		std::vector<olc_v2d> helper_path = scene.trace(player_ray, player.max_helper_distance);

		// Ray trace tongue path
		std::vector<olc_v2d> tongue_path = scene.trace(player_ray, std::min(player.current_tongue_distance, player.max_tongue_distance));

		olc_v2d tongue_tip;
		if (!tongue_path.empty())
			tongue_tip = tongue_path.back();

		int n_tongue_bounces = tongue_path.size() < 2 ? 0 : tongue_path.size() - 2;

		// Populate "catchable" part of tongue
		std::vector<olc_line> catchable_tongue_path;
		if (player.tongue.active)
		{
			T catchable_distance = player.max_tongue_distance * 0.1;
			for (int i = tongue_path.size() - 1; i - 1 >= 0; i--)
			{
				olc_line line(tongue_path[i], tongue_path[i - 1]);
				T line_mag = line.length();
				if (line_mag <= catchable_distance)
				{
					catchable_tongue_path.push_back(line);
					catchable_distance -= line_mag;
				}
				else
				{
					// Add remaining distance
					olc_v2d point = line.rpoint(catchable_distance);
					catchable_tongue_path.push_back(olc_line(tongue_path[i], point));
					break;
				}
			}
		}

		// Update enemies and player
		for (const std::unique_ptr<Enemy> &enemy : enemies)
		{
			if (enemy->is_caught())
			{
				if (player.tongue.active)
				{
					if (player.abilities.piercing_tip)
					{
						// Enemy stays on tongue at location it was caught during extension, and gets reeled back in
						if (player.current_tongue_distance < enemy->piercing_tip_caught_distance)
						{
							enemy->pos = tongue_tip;
						}
						else
						{
							T total_distance;
							for (int i = 0; i + 1 < tongue_path.size(); i++)
							{
								olc_line line(tongue_path[i], tongue_path[i + 1]);
								if (enemy->piercing_tip_caught_distance <= line.length() + total_distance)
								{
									enemy->pos = line.rpoint(enemy->piercing_tip_caught_distance - total_distance);
									break;
								}
								total_distance += line.length();
							}
						}
					}
					else
					{
						enemy->pos = tongue_tip;
					}
				}

				continue;
			}

			enemy->behavior(fElapsedTime, player);

			// enemy->expire_time -= fElapsedTime;

			olc_v2d new_pos = enemy->pos + enemy->direction * enemy->movement_speed * fElapsedTime;
			olc_ray reflected_ray;

			// Should always hit at least the bounding box of the scene
			// If not, the object's new position has moved out of the scene and we don't want to update to that position
			// (maybe due to a long lag between frames)
			bool is_inside_scene_bounds = scene.closest_reflection(olc_ray(new_pos, enemy->direction), reflected_ray);

			if (is_inside_scene_bounds)
			{
				bool collides = false;
				// bool collides_with_scene = false;
				// bool collides_with_player = false;
				// bool collides_with_enemy = false;

				if (player.tongue.active && player.abilities.magnet_reel)
				{
					// Players get pulled into tongue location
					olc_v2d vec_between = (tongue_tip - enemy->pos);
					if (vec_between.mag() < MAGNET_REEL_REACH)
					{
						new_pos += vec_between.norm() * MAGNET_REEL_STRENGTH * ((MAGNET_REEL_REACH - vec_between.mag()) / MAGNET_REEL_REACH) * fElapsedTime;
					}
				}

				// Collision with scene
				if (enemy->check_contains(reflected_ray.origin))
				{
					enemy->direction = reflected_ray.direction;
					collides = true;
				}

				// std::optional<olc_v2d> reflected_direction = scene.reflect_object(*enemy);
				// if (reflected_direction)
				// {
				// 	enemy->direction = *reflected_direction;
				// 	collides_with_scene = true;
				// }

				// Collision with player
				if (!collides)
				{
					// TODO collision with player (maybe makes player spin out to lose some time, if a triangel bug)
					// TODO OTHERS, only reflect but no spin
					std::optional<olc_ray> r = player.get_reflectable_surface()->reflect(olc_ray(new_pos, player.pos - enemy->pos));
					if (r && enemy->check_contains(r->origin))
					{
						if (!player.is_hit())
						{
							int points_lost_on_hit = enemy->points_lost_on_hit();
							if (points_lost_on_hit > 0)
							{
								add_points(-points_lost_on_hit);
							}

							enemy->hit_player(player);
						}

						enemy->direction = r->direction.norm();
						collides = true;
					}
				}

				// Collision with other enemies
				if (!collides)
				{
					for (const std::unique_ptr<Enemy> &other : enemies)
					{
						if (enemy == other)
							continue;

						std::optional<olc_ray> reflected_ray = other->get_reflectable_surface()->reflect(olc_ray(new_pos, other->pos - enemy->pos));
						if (reflected_ray && enemy->check_contains(reflected_ray->origin))
						{
							// Hit the other enemy
							enemy->direction = reflected_ray->direction.norm();
							collides = true;
							break;
						}
					}
				}

				if (collides)
				{
					enemy->on_collision();
				}
				else
				{
					enemy->pos = new_pos;
				}
			}

			// Check for tongue hit
			if (player.tongue.active)
			{
				bool enemy_hit = false;
				for (const olc_line &catchable_line : catchable_tongue_path)
				{
					if (enemy->check_overlaps(catchable_line))
					{
						enemy_hit = true;
						break;
					}
				}

				if (player.abilities.bubble_tongue)
				{
					enemy_hit |= enemy->check_overlaps(olc_circle(tongue_tip, BUBBLE_TONGUE_SIZE));
				}

				if (enemy_hit)
				{
					if (!tongue_debounce || player.abilities.piercing_tip)
					{
						enemy->on_tongue_hit(n_tongue_bounces);
						tongue_debounce = true;
					}

					// Piercing tip can go through multiple enemies
					if (player.abilities.piercing_tip)
					{
						enemy->piercing_tip_caught_distance = player.current_tongue_distance;
					}
					else
					{
						player.tongue.engaging = false;
						player.tongue.retracting = true;
					}
				}
			}
		}

/* === DRAWING === */
#pragma region DRAWING

		// Draw scene objects
		scene.draw(this);

		// Draw tongue
		if (player.tongue.active)
		{
			// Draw tongue path
			for (int i = 0; i + 1 < tongue_path.size(); i++)
			{
				DrawLine(tongue_path[i], tongue_path[i + 1], olc::DARK_MAGENTA);

				if (i + 1 != tongue_path.size() - 1)
				{
					// Draw each tongue bounce as a circle with a +1 next to it
					DrawCircle(tongue_path[i + 1], 5, olc::CYAN);
					DrawString(tongue_path[i + 1] + olc_v2d(8, 0), "+" + std::to_string(i + 1), olc::CYAN);
				}
			}

			// Draw catchable part of tongue path
			for (const olc_line &line : catchable_tongue_path)
			{
				DrawLine(line.start, line.end, olc::MAGENTA);
			}

			// Draw tongue tip
			if (player.abilities.piercing_tip)
			{
				if (!catchable_tongue_path.empty())
				{
					int size = 8;
					const olc_line &last_line = catchable_tongue_path.front();
					olc_v2d last_tongue_segment_direction = last_line.vector().norm();
					olc_v2d center_point = tongue_tip + last_tongue_segment_direction * size;
					olc_v2d perp_vector = last_tongue_segment_direction.perp() * size / 2;
					FillTriangle(tongue_tip, center_point + perp_vector, center_point - perp_vector, olc::GREY);
				}
			}
			else if (player.abilities.magnet_reel)
			{
				if (!catchable_tongue_path.empty())
				{
					int size = 12;
					const olc_line &last_line = catchable_tongue_path.front();
					FillRotatedRectDecal(this, tongue_tip, olc_v2d(size, size), last_line.vector().polar().y, olc::GREY);

					static float animation_timer = 0;
					static int frame = 0;
					static const int n_frames = 20;
					if (animation_timer <= 0)
					{
						animation_timer = 0.025;
						frame = ++frame % n_frames;
					}
					DrawCircle(tongue_tip, 5 + (n_frames - frame) * 2.5, olc::CYAN);
					animation_timer -= fElapsedTime;
				}
			}
			else
			{
				int size = player.abilities.bubble_tongue ? BUBBLE_TONGUE_SIZE : 2;
				FillCircle(tongue_tip, size, olc::MAGENTA);
			}
		}

		// Draw enemies
		for (int i = 0; i < enemies.size(); i++)
		{
			enemies[i]->draw(this, fElapsedTime);
		}

		// Draw helper path
		if (player.current_tongue_distance < player.max_helper_distance)
		{
			for (int i = 0; i + 1 < helper_path.size(); i++)
			{
				DrawLine(helper_path[i], helper_path[i + 1], olc::CYAN, 0x11111111);
			}
		}

		// Draw player
		player.draw(this, fElapsedTime);

		// === Draw Text and HUD ===
		if (!game_over)
		{
			FillRect(scene_bounds.pos.x, 0, scene_bounds.size.x, scene_bounds.pos.y, olc::VERY_DARK_GREY);
			DrawRect(scene_bounds.pos.x, 0, scene_bounds.size.x, scene_bounds.pos.y, olc::GREY);

			int start_x = scene_bounds.pos.x;
			int bottom_y = ScreenHeight() - 12;
			DrawString(olc::vi2d(start_x, bottom_y), "Move: W/A/S/D", olc::WHITE);
			DrawString(olc::vi2d(start_x + 140, bottom_y), "Aim and Catch Shapes: Mouse", olc::WHITE);
			DrawString(olc::vi2d(start_x + 780, bottom_y), "Help / How to Play: F1", olc::WHITE);

			// TODO maybe add in a keyboard only mode

			int wave_counter_x = scene_bounds.pos.x + 5;
			DrawStringDecal(olc::vi2d(wave_counter_x, 5), "WAVE", olc::WHITE, {1.5, 1.5});
			DrawStringDecal(olc::vi2d(wave_counter_x + 10, 25), std::to_string(game_state.wave_number + 1), olc::WHITE, {2.5, 2.5});
			DrawStringDecal(olc::vi2d(wave_counter_x + 30, 40), "/" + std::to_string(N_WAVES), olc::WHITE, {0.8, 0.8});

			DrawLineDecal(olc::vi2d(scene_bounds.pos.x + 80, 5), olc::vi2d(scene_bounds.pos.x + 80, scene_bounds.pos.y - 5), olc::WHITE);

			int target_shape_str_x = scene_bounds.pos.x + 100;
			DrawStringDecal(olc::vi2d(target_shape_str_x, 5), "TARGET SHAPE", olc::WHITE, {1.5, 1.5});
			DrawStringDecal(olc::vi2d(target_shape_str_x + 150, 5), "(x2)", olc::WHITE, {1.0, 1.0});
			int target_shape_x = target_shape_str_x + 40;
			int target_shape_y = 40;
			int target_shape_size = 18;
			olc::Pixel target_shape_color = olc::WHITE;

			if (game_state.wave_timer < 3.0)
			{
				static float animation_timer = 0;
				static bool animation_dir = false;
				static float t = 0;
				animation_timer -= fElapsedTime;
				if (animation_timer <= 0)
				{
					animation_timer = 0.01;
					t = animation_dir ? t + 0.1 : t - 0.1;
					if (t < 0 || t > 1)
					{
						animation_dir = !animation_dir;
						t = std::clamp(t, 0.0f, 1.0f);
					}
				}
				SetPixelMode(olc::Pixel::ALPHA);
				target_shape_color = olc::PixelLerp(olc::GREEN, olc::BLACK, t);
			}

			switch (game_state.current_target_shape)
			{
			case ShapeId::CIRCLE:
				FillCircle(olc::vi2d(target_shape_x, target_shape_y), target_shape_size, target_shape_color);
				break;
			case ShapeId::RECTANGLE:
				FillRect(olc::vi2d(target_shape_x - target_shape_size, target_shape_y - target_shape_size), olc::vi2d(target_shape_size * 2, target_shape_size * 2), target_shape_color);
				break;
			case ShapeId::TRIANGLE:
				FillTriangle(olc::vi2d(target_shape_x, target_shape_y - target_shape_size), olc::vi2d(target_shape_x - target_shape_size, target_shape_y + target_shape_size), olc::vi2d(target_shape_x + target_shape_size, target_shape_y + target_shape_size), target_shape_color);
				break;
			default:
				DrawStringDecal(olc::vi2d(target_shape_x, target_shape_y - 8), "Warm up...", olc::WHITE, {1.2, 1.2});
				break;
			}
			SetPixelMode(olc::Pixel::NORMAL);

			if (game_state.current_target_shape != ShapeId::NONE)
			{
				int pip_x = target_shape_x + 45;
				int pip_y = 40;
				int pip_radius = 7;
				int pip_spacing = 22;
				int pips_filled = std::min(game_state.current_target_shape_streak, N_TARGET_CATCHES_NEEDED);
				for (int i = 0; i < N_TARGET_CATCHES_NEEDED; ++i)
				{
					olc::Pixel pip_col = (i < pips_filled) ? olc::YELLOW : olc::DARK_GREY;
					if (game_state.current_target_shape_streak >= N_TARGET_CATCHES_NEEDED)
						pip_col = olc::GREEN;
					FillCircle(olc::vi2d(pip_x + i * pip_spacing, pip_y), pip_radius, pip_col);
					DrawCircle(olc::vi2d(pip_x + i * pip_spacing, pip_y), pip_radius, olc::WHITE);
				}
			}

			DrawLineDecal(olc::vi2d(scene_bounds.pos.x + 290, 5), olc::vi2d(scene_bounds.pos.x + 290, scene_bounds.pos.y - 5), olc::WHITE);

			// TODO wave counter bar

			int current_multiplier = get_current_multiplier();

			olc::Pixel total_multiplier_color = olc::WHITE;
			if (current_multiplier >= 3)
			{
				total_multiplier_color = olc::PixelLerp(olc::YELLOW, olc::RED, (current_multiplier - 3) / 9.0f);
			}
			if (current_multiplier >= 6)
			{
				static float animation_timer = 0;
				static bool animation_dir = false;
				static float t = 0;
				animation_timer -= fElapsedTime;
				if (animation_timer <= 0)
				{
					animation_timer = 0.01;
					t = animation_dir ? t + 0.1 : t - 0.1;
					if (t < 0 || t > 1)
					{
						animation_dir = !animation_dir;
						t = std::clamp(t, 0.0f, 1.0f);
					}
				}
				total_multiplier_color = olc::PixelLerp(olc::YELLOW, olc::RED, t);
			}

			int multiplier_start_x = scene_bounds.pos.x + scene_bounds.size.x / 2 - 180;
			int multiplier_start_y = 5;
			DrawStringDecal(olc::vi2d(multiplier_start_x, multiplier_start_y), "MULTIPLIERS", olc::WHITE, {1.45, 1.45});
			DrawStringDecal(olc::vi2d(multiplier_start_x + 10, multiplier_start_y + 20), "Time Combo:    " + std::to_string(game_state.current_combo_streak.multiplier) + "x", olc::WHITE);
			DrawStringDecal(olc::vi2d(multiplier_start_x + 10, multiplier_start_y + 35), "Target Streak: " + std::to_string(game_state.current_target_streak.multiplier) + "x", olc::WHITE);
			// DrawStringDecal(olc::vi2d(multiplier_start_x + 20, multiplier_start_y + 45), "Shape Ring:    " + std::to_string(game_state.shape_ring_multiplier_active ? SHAPE_RING_MULTIPLIER : 0) + "x", olc::WHITE);
			DrawStringDecal(olc::vi2d(multiplier_start_x + 160, multiplier_start_y + 20), "= " + std::to_string(current_multiplier) + "x", total_multiplier_color, {2.2, 2.2});

			DrawLineDecal(olc::vi2d(multiplier_start_x + 240, 5), olc::vi2d(multiplier_start_x + 240, scene_bounds.pos.y - 5), olc::WHITE);

			int ability_x = scene_bounds.pos.x + scene_bounds.size.x / 2 + 70;
			std::string ability_str;
			if (player.abilities.bubble_tongue)
				ability_str = "Bubble Tongue";
			else if (player.abilities.magnet_reel)
				ability_str = "Magnet Reel";
			else if (player.abilities.piercing_tip)
				ability_str = "Piercing Tip";
			else
				ability_str = "None";
			DrawStringDecal(olc::vi2d(ability_x, 5), "Current Ability", olc::WHITE, {1.3, 1.3});
			DrawStringDecal(olc::vi2d(ability_x, 30), ability_str, olc::WHITE, {1.1, 1.1});

			DrawLineDecal(olc::vi2d(ability_x + 170, 5), olc::vi2d(ability_x + 170, scene_bounds.pos.y - 5), olc::WHITE);

			olc::vi2d score_location(scene_bounds.pos.x + scene_bounds.size.x - 110, 20);
			DrawStringPropDecal(olc_v2d(score_location.x - 130, score_location.y), "Score:", olc::WHITE, {3.0, 3.0});
			DrawStringPropDecal(score_location, std::to_string(game_state.score), olc::GREEN, {3.0, 3.0});
		}

		/* === UPDATE GAME STATE === */
#pragma region UPDATE GAME STATE

		game_state.current_combo_streak.window_time -= fElapsedTime;

		if (get_current_multiplier() > game_state.stats.highest_multiplier)
		{
			game_state.stats.highest_multiplier = get_current_multiplier();
		}

		if (game_state.current_target_shape_streak > game_state.stats.highest_shape_streak)
		{
			game_state.stats.highest_shape_streak = game_state.current_target_shape_streak;
		}

		// === Cleanup any caught enemies and tally points
		if (!player.tongue.active)
		{
			int total_points_gained = 0;

			auto process_caught_enemy = [&](const std::unique_ptr<Enemy> &enemy)
			{
				game_state.stats.enemies_caught++;

				if (game_state.stats.highest_tongue_bounces_catch < enemy->n_tongue_bounces_when_caught)
				{
					game_state.stats.highest_tongue_bounces_catch = enemy->n_tongue_bounces_when_caught;
				}

				// if (game_state.current_target_streak.shape == ShapeId::NONE)
				// 	game_state.current_target_streak.shape = enemy->get_shape_id();

				// TODO shape streak should be per-shape and persistent

				bool continue_target_streak = enemy->get_shape_id() == game_state.current_target_shape;
				if (!continue_target_streak)
				{
					game_state.current_target_streak.multiplier = 0;
					// game_state.current_target_streak.shape = enemy->get_shape_id();
				}

				int points = enemy->points_worth;
				if (enemy->get_shape_id() == game_state.current_target_shape)
					points *= 2;

				points += enemy->n_tongue_bounces_when_caught;

				points *= get_current_multiplier();

				// Rack in those points
				total_points_gained += points;

				// Any catch resets combo timer
				game_state.current_combo_streak.window_time = COMBO_MULTIPLIER_WINDOW_TIME_SEC;

				game_state.current_combo_streak.multiplier += 1;
				if (game_state.current_combo_streak.multiplier > MAX_COMBO_MULTIPLIER)
					game_state.current_combo_streak.multiplier = MAX_COMBO_MULTIPLIER;

				if (continue_target_streak)
				{
					game_state.current_target_streak.multiplier += 1;
					if (game_state.current_target_streak.multiplier > MAX_TARGET_STREAK_MULTIPLIER)
						game_state.current_target_streak.multiplier = MAX_TARGET_STREAK_MULTIPLIER;
				}

				// Wave Target Shape
				if (enemy->get_shape_id() == game_state.current_target_shape)
				{
					game_state.current_target_shape_streak += 1;
				}
			};

			enemies.erase(std::remove_if(enemies.begin(), enemies.end(),
										 [&](const std::unique_ptr<Enemy> &enemy)
										 {
											 if (enemy->is_caught())
											 {
												 process_caught_enemy(enemy);
												 return true;
											 }
											 return false;
										 }),
						  enemies.end());

			if (total_points_gained > 0)
			{
				// TODO: Play sound effect for points gained

				add_points(total_points_gained);
			}
		}

		if (new_points_timer > 0.0)
		{
			new_points_timer -= fElapsedTime;

			SetPixelMode(olc::Pixel::ALPHA);
			std::string sign = new_points_to_show >= 0 ? "+" : "";
			olc::Pixel base_col = new_points_to_show >= 0 ? olc::CYAN : olc::RED;
			olc::Pixel col = olc::PixelLerp(olc::BLANK, base_col, new_points_timer);
			DrawStringDecal(score_location + olc::vi2d(102, -14), sign + std::to_string(new_points_to_show), col, {2.0, 2.0});
			SetPixelMode(olc::Pixel::NORMAL);
		}

		// Reset after caught enemy evaluation to ensure processessing matches shown values for this frame
		if (game_state.current_combo_streak.window_time <= 0.0)
		{
			game_state.current_combo_streak.window_time = 0.0;
			game_state.current_combo_streak.multiplier = 0;
		}

		// ===== Update Wave =====
		game_state.wave_timer += fElapsedTime;

		if (game_state.wave_timer >= WAVE_LENGTH)
		{
			if (game_state.wave_number + 1 < N_WAVES)
				on_new_wave();
			else
				game_over = true;
		}

		if (game_state.current_target_shape_streak >= N_TARGET_CATCHES_NEEDED)
		{
			// Spawn shape ring
			switch (game_state.current_target_shape)
			{
			case ShapeId::CIRCLE:
				player.abilities.bubble_tongue = true;
				player.abilities.magnet_reel = false;
				player.abilities.piercing_tip = false;
				break;
			case ShapeId::RECTANGLE:
				player.abilities.magnet_reel = true;
				player.abilities.bubble_tongue = false;
				player.abilities.piercing_tip = false;
				break;
			case ShapeId::TRIANGLE:
				player.abilities.piercing_tip = true;
				player.abilities.bubble_tongue = false;
				player.abilities.magnet_reel = false;
				break;
			}
		}

		bool spawn_new_enemy = false;
		enemy_spawn_timer += fElapsedTime;
		if (enemy_spawn_timer > ENEMY_SPAWN_INTERVAL)
		{
			enemy_spawn_timer = 0.0f;
			spawn_new_enemy = true;
		}

		// Spawn new enemies
		if (enemies.size() < (5 + game_state.wave_number * 2) || spawn_new_enemy)
		{
			ShapeId new_shape = random_shape_id();
			if (game_state.current_target_shape != ShapeId::NONE)
			{
				if (std::none_of(enemies.begin(), enemies.end(), [&](const std::unique_ptr<Enemy> &e)
								 { return e->get_shape_id() == game_state.current_target_shape; }))
				{
					new_shape = game_state.current_target_shape;
				}
			}

			int buffer = 20;
			olc_v2d location = olc_v2d(rand_int(scene_bounds.pos.x + buffer, scene_bounds.pos.x + scene_bounds.size.x - buffer),
									   rand_int(scene_bounds.pos.y + buffer, scene_bounds.pos.y + scene_bounds.size.y - buffer));
			switch (new_shape)
			{
			case ShapeId::CIRCLE:
				enemies.push_back(std::make_unique<CircleFly>(location));
				break;
			case ShapeId::RECTANGLE:
				enemies.push_back(std::make_unique<SquareBeetle>(location));
				break;
			case ShapeId::TRIANGLE:
				enemies.push_back(std::make_unique<TriangleWasp>(location));
				break;
			}
		}

		if (game_over)
		{
			SetPixelMode(olc::Pixel::ALPHA);
			FillRect(0, 0, ScreenWidth(), ScreenHeight(), olc::Pixel(0, 0, 0, 180));
			SetPixelMode(olc::Pixel::NORMAL);

			std::string game_over_text = "GAME OVER";
			std::string score_text = "Final Score: " + std::to_string(game_state.score);
			std::string shapes_caught_text = "Shapes Caught:                            " + std::to_string(game_state.stats.enemies_caught);
			std::string highest_multiplier_text = "Highest Multiplier:                       " + std::to_string(game_state.stats.highest_multiplier);
			std::string highest_shape_streak_text = "Highest Target Streak:                    " + std::to_string(game_state.stats.highest_shape_streak);
			std::string highest_tongue_bounces_caught_text = "Highest Tongue Bounces in a Single Catch: " + std::to_string(game_state.stats.highest_tongue_bounces_catch);
			std::string restart_text = "Press SPACE to Restart";

			int centerX = ScreenWidth() / 2;
			int centerY = ScreenHeight() / 2;

			DrawStringPropDecal(olc_v2d(centerX - 200, centerY - 150), game_over_text, olc::RED, {6.0f, 6.0f});
			DrawStringPropDecal(olc_v2d(centerX - 170, centerY - 40), score_text, olc::GREEN, {4.0f, 4.0f});
			DrawStringDecal(olc_v2d(centerX - 420, centerY + 60), shapes_caught_text, olc::WHITE, {2.5f, 2.5f});
			DrawStringDecal(olc_v2d(centerX - 420, centerY + 110), highest_multiplier_text, olc::WHITE, {2.5f, 2.5f});
			DrawStringDecal(olc_v2d(centerX - 420, centerY + 160), highest_shape_streak_text, olc::WHITE, {2.5f, 2.5f});
			DrawStringDecal(olc_v2d(centerX - 420, centerY + 210), highest_tongue_bounces_caught_text, olc::WHITE, {2.5f, 2.5f});
			DrawStringPropDecal(olc_v2d(centerX - 140, centerY + 280), restart_text, olc::YELLOW, {1.8f, 1.8f});
		}

		return true;
	}

	void init_player()
	{
		T random_angle = rand_int(0, 360);
		player.pos = scene_bounds.middle();
		player.tongue_direction = (olc_v2d(1, random_angle * M_PI / 180.0)).cart().norm();
	}

	void add_points(int points)
	{
		if (game_over)
			return;
		new_points_timer = 1.0f;
		new_points_to_show = points;
		game_state.score += points;
	}

	void on_new_wave()
	{
		if (game_over)
			return;
		game_state.wave_number++;
		game_state.wave_timer = 0.0f;

		game_state.current_target_shape = random_shape_id();
		game_state.current_target_shape_streak = 0;

		// game_state.shape_ring_active = false;
	}

	int get_current_multiplier()
	{
		int multiplier = 1;
		multiplier += game_state.current_combo_streak.multiplier;
		multiplier += game_state.current_target_streak.multiplier;

		// if (game_state.shape_ring_multiplier_active)
		// multiplier += SHAPE_RING_MULTIPLIER;

		return multiplier;
	}

	void reset_game()
	{
		game_over = false;
		game_state = GameState();
		player = Player();
		init_player();
		enemies.clear();

		new_points_timer = 0;
		new_points_to_show = 0;
		enemy_spawn_timer = 0;
	}
};

int main()
{
	OlcCodeJam2025 demo;
	if (demo.Construct(1024, 720, 2, 2, false, true))
		demo.Start();
	return 0;
}
