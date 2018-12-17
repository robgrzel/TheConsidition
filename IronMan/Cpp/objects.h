//
// Created by user on 11/14/2018.
//

#ifndef THECONSIDITION_OBJECTS_H
#define THECONSIDITION_OBJECTS_H

#include <iostream>
#include <unordered_map>
#include <string>
#include <vector>
#include <assert.h>

#define elif else if
#define DEBUG

/*
Instant
RemoveCloud – Removes rain tiles around your players position. A 10x10 tile area with your player in the middle is cleared of bad weather.
RestoreStamina – Instantly sets your players stamina to full
InvertStreams – Inverts the direction of all waterstreams (South => North, East => West and vice versa) in a 50x50 tile area with your player in the middle.

Duration
Shoes, 10 turns – Trail tiles deduct 25% less movement points
Flippers, 10 turns – Water tiles deduct 25% less movement points
Cycletire, 10 turns – Road tiles deduct 25% less movement points
Umbrella, 25 turns – Your player is immune to the additional stamina deduction caused by rain tiles
Energyboost, 10 turns – You regain 10 more stamina per turn
Potion, 5 turns – You gain 50% more movement points
Helmet, 25 turns – You become immune to stuns
StaminaSale, 10 turns – You consume 40% less stamina
Spikeshoes, 10 turns – Ignore the effect of deviation on trail tiles (running)
Cyklop, 10 turns – Ignore the effect of deviation on water tiles (swimming)
BicycleHandlebar, 10 turns – Ignore the effect of deviation on road tiles (biking)
 */


enum directions {
	w_e = 400,
	s_e = 500,
	e_e = 600,
	n_e = 700,
	aoe_e = 800,
	null_dir_e = 0,
};


enum boost_type {
	remove_rain_e = 10, //10x10
	full_stamina_restore_e = 11,
	inverse_streams_e = 12, //50x50, s->n, e->w, n->s, w->e
	less_mp_percent_e = 13, //10turns, 25%
	stamina_immune_to_rain_e = 14,
	regain_stamina_per_tour_e = 15, //10turns, 10/t
	regain_mp_percent_per_tour_e = 16,
	immune_to_stun_e = 17,
	consume_less_stamina_e = 18,   //40%
	ignore_deviation_e = 19, //10turns
	null_boost_e = 0
};

enum terrain_types {
	road_e = 1000,
	trail_e = 1100,
	grass_e = 1200,
	water_e = 1300,
	rockywater_e = 1400,
	forest_e = 1500,
	start_e = 1600,
	win_e = 1700,
	null_terrain_e = 0,
};

enum terrain_dynamics {
	rain_e = 10000,
	elevation_e = 20000,
	waterstream_e = 30000,
	powerup_e = 40000,
	nulldyn_e = 0,
};

enum player_states {
	stamina_e = 100000,
	movementpoints_e = 200000,
	stuns_e = 300000,
	deviation_e = 400000,
	null_effect_e = 0,
};


enum item_types {
	instant_item_e = 1000000,
	duration_item_e = 2000000,
	removecloud_e = instant_item_e + rain_e + aoe_e + remove_rain_e,
	restorestamina_e = instant_item_e + stamina_e + full_stamina_restore_e,
	invertstreams_e = instant_item_e + waterstream_e + inverse_streams_e,
	shoes_e = duration_item_e + movementpoints_e + trail_e + less_mp_percent_e,
	flippers_e = duration_item_e + movementpoints_e + water_e + less_mp_percent_e,
	cycletire_e = duration_item_e + movementpoints_e + road_e + less_mp_percent_e,
	umbrella_e = duration_item_e + stamina_e + rain_e + stamina_immune_to_rain_e,
	energyboots_e = duration_item_e + stamina_e + regain_stamina_per_tour_e,
	potion_e = duration_item_e + movementpoints_e + regain_mp_percent_per_tour_e,
	helmet_e = duration_item_e + stuns_e + immune_to_stun_e,
	staminasale_e = duration_item_e + stamina_e + consume_less_stamina_e,
	spikeshoes_e = duration_item_e + deviation_e + trail_e + ignore_deviation_e,
	cyklop_e = duration_item_e + deviation_e + water_e + ignore_deviation_e,
	bicyclehandlebar_e = duration_item_e + deviation_e + road_e + ignore_deviation_e,
	null_item_e = 0
};


typedef struct item_properties {
	std::string name;
	boost_type boosttype;
	terrain_types terrtype;
	
} item_properties_t;

class ItemsNames {
	std::unordered_map<int, std::string> umap;

public:
	ItemsNames();
	
	auto get(int key);
	
	
};

class ItemsProperties {
	std::unordered_map<int, std::vector<int>> umap;

public:
	ItemsProperties();
	
	auto get(int key);
	
	
};

static ItemsNames itemsNamesMap = ItemsNames();

class Item {
public:
	item_types itemType = null_item_e;
	item_types item = null_item_e;
	player_states effect = null_effect_e;
	terrain_dynamics terrdyn = nulldyn_e;
	terrain_types terrtype = null_terrain_e;
	directions direction = null_dir_e;
	boost_type boost = null_boost_e;
	
	int turns;
	int turnsLeft;
	
	int itemReduced = 0;
	
	std::string itemName = "null";
	std::string itemTypeName = "null";
	std::string effectName = "null";
	std::string terrdynName = "null";
	std::string terrtypeName = "null";
	std::string directionName = "null";
	std::string boostName = "null";

public:
	explicit Item(item_types item_);
	
	item_types get_type();
	
	terrain_dynamics get_terrdyn();
	
	terrain_types get_terrtype();
	
	directions get_direction();
	
	player_states get_effect();
	
	boost_type get_boost();
	
	std::string get_info(int item);
	
	
};


class Player {
	
	boost_type boosts[3];
	int eq[3];
	int mp;
	int stamina;
	int turn;
	
	int drop_item(int idx){
	
	}
	
	int pick_item(int item){
	
	}
	
	int apply_effect(Item item) {
		
		boost_type boost = item.boost;
		
		if (boost == remove_rain_e)   remove_rain(item);
		elif (boost == full_stamina_restore_e)   restore_stamina(item);
		elif (boost == inverse_streams_e)   restore_stamina(item);
		elif (boost == less_mp_percent_e)   inverse_stream(item);
		elif (boost == stamina_immune_to_rain_e)   less_mp_percent(item);
		elif (boost == regain_stamina_per_tour_e)   stamina_immune_to_rain(item);
		elif (boost == regain_mp_percent_per_tour_e)   regain_stamina_per_tour(item);
		elif (boost == immune_to_stun_e)   immune_to_stun(item);
		elif (boost == consume_less_stamina_e)   consume_less_stamina(item);
		elif (boost == ignore_deviation_e)   ignore_deviation(item);
		else return -1;
		
		return 0;
	}
	
	int remove_rain(Item item) {
	
		return 0;
	}
	
	int restore_stamina(Item item) {
		return 0;
	}
	
	int inverse_stream(Item item) {
		return 0;
	}
	
	int less_mp_percent(Item item) {
		return 0;
	}
	
	int stamina_immune_to_rain(Item item) {
		return 0;
	
	}
	
	int regain_stamina_per_tour(Item item) {
		return 0;
	
	}
	
	int immune_to_stun(Item item) {
		return 0;
	
	}
	
	int consume_less_stamina(Item item) {
		return 0;
	
	}
	
	int ignore_deviation(Item item) {
		return 0;
	
	}
	
	
};


#endif //THECONSIDITION_OBJECTS_H
