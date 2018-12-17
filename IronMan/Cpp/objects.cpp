#include "objects.h"

ItemsNames::ItemsNames() {
	
	umap[0] = "null";
	umap[w_e] = "w";
	umap[s_e] = "s";
	umap[e_e] = "e";
	umap[n_e] = "n";
	umap[aoe_e] = "aoe";
	
	umap[stamina_e] = "stamina";
	umap[movementpoints_e] = "movementpoints";
	umap[stuns_e] = "stuns";
	umap[deviation_e] = "deviation";
	
	umap[rain_e] = "rain";
	umap[elevation_e] = "elevation";
	umap[waterstream_e] = "waterstream";
	umap[powerup_e] = "powerup";
	
	umap[road_e] = "road";
	umap[trail_e] = "trail";
	umap[grass_e] = "grass";
	umap[water_e] = "water";
	umap[rockywater_e] = "rockywater";
	umap[forest_e] = "forest";
	umap[start_e] = "start";
	umap[win_e] = "win";
	
	umap[instant_item_e] = "instant_item";
	umap[duration_item_e] = "duration_item";
	umap[removecloud_e] = "removecloud";
	umap[restorestamina_e] = "restorestamina";
	umap[invertstreams_e] = "inverstreams";
	umap[shoes_e] = "shoes";
	umap[flippers_e] = "flippers";
	umap[cycletire_e] = "cycletire";
	umap[umbrella_e] = "umbrella";
	umap[energyboots_e] = "energyboots";
	umap[potion_e] = "potion";
	umap[helmet_e] = "helmet";
	umap[staminasale_e] = "staminasale";
	umap[spikeshoes_e] = "spikeshoes";
	umap[cyklop_e] = "cyklop";
	umap[staminasale_e] = "staminasale";
	umap[bicyclehandlebar_e] = "bicyclehandlebar";
	
	umap[remove_rain_e] = "remove_rain_e";
	umap[full_stamina_restore_e] = "full_stamina_restore_e";
	umap[inverse_streams_e] = "inverse_streams_e";
	umap[less_mp_percent_e] = "less_mp_percent_e";
	umap[stamina_immune_to_rain_e] = "stamina_immune_to_rain_e";
	umap[regain_stamina_per_tour_e] = "regain_stamina_per_tour_e";
	umap[regain_mp_percent_per_tour_e] = "regain_mp_percent_per_tour_e";
	umap[immune_to_stun_e] = "immune_to_stun_e";
	umap[consume_less_stamina_e] = "consume_less_stamina_e";
	umap[ignore_deviation_e] = "ignore_deviation_e";
	
}


auto ItemsNames::get(int key) {
	return umap[key];
}


auto ItemsProperties::get(int key) {
	return umap[key];
}


Item::Item(item_types item_) {
	item = item_;
	itemReduced = item_;
	itemType = get_type();
	effect = get_effect();
	terrdyn = get_terrdyn();
	terrtype = get_terrtype();
	direction = get_direction();
	boost = get_boost();
	
	assert (itemReduced == 0);
	
	#ifdef DEBUG
	
	itemName = get_info(item);
	itemTypeName = get_info(itemType);
	effectName = get_info(effect);
	terrdynName = get_info(terrdyn);
	terrtypeName = get_info(terrtype);
	directionName = get_info(direction);
	boostName = get_info(boost);
	printf("itemReduced %d, \n", itemReduced);
	
	#endif
}

item_types Item::get_type() {
	if (item >= duration_item_e) itemType = duration_item_e;
	elif (item >= instant_item_e) itemType = instant_item_e;
	itemReduced -= itemType;
	return itemType;
}

terrain_dynamics Item::get_terrdyn() {
	
	auto terrdyn_ = (terrain_dynamics) (itemReduced);
	
	if (terrdyn_ >= powerup_e) terrdyn = powerup_e;
	elif (terrdyn_ >= waterstream_e) terrdyn = waterstream_e;
	elif (terrdyn_ >= elevation_e) terrdyn = elevation_e;
	elif (terrdyn_ >= rain_e) terrdyn = rain_e;
	
	
	printf("terrdyn %d, \n", terrdyn);
	itemReduced -= terrdyn;
	
	return terrdyn;
	
}

terrain_types Item::get_terrtype() {
	
	auto terrtype_ = (terrain_types) (itemReduced);
	
	if (terrtype_ >= win_e) terrtype = win_e;
	elif (terrtype_ >= start_e) terrtype = start_e;
	elif (terrtype_ >= forest_e) terrtype = forest_e;
	elif (terrtype_ >= rockywater_e) terrtype = rockywater_e;
	elif (terrtype_ >= water_e) terrtype = water_e;
	elif (terrtype_ >= grass_e) terrtype = grass_e;
	elif (terrtype_ >= trail_e) terrtype = trail_e;
	elif (terrtype_ >= road_e) terrtype = road_e;
	
	printf("terrtype %d, \n", terrtype);
	itemReduced -= terrtype;
	
	return terrtype;
	
}


directions Item::get_direction() {
	auto direction_ = (directions) (itemReduced);
	
	if (direction_ >= aoe_e) direction = aoe_e;
	elif (direction_ >= n_e) direction = n_e;
	elif (direction_ >= e_e) direction = e_e;
	elif (direction_ >= s_e) direction = s_e;
	elif (direction_ >= w_e) direction = w_e;
	
	printf("terrtype %d, \n", terrtype);
	itemReduced -= direction;
	
	
	return direction;
	
}

player_states Item::get_effect() {
	auto effect_ = (player_states) (itemReduced);
	
	if (effect_ >= deviation_e) effect = deviation_e;
	elif (effect_ >= stuns_e) effect = stuns_e;
	elif (effect_ >= movementpoints_e) effect = movementpoints_e;
	elif (effect_ >= stamina_e) effect = stamina_e;
	printf("effect %d, \n", effect);
	
	itemReduced -= effect;
	
	return effect;
	
}

boost_type Item::get_boost() {
	auto boost_ = (boost_type) (itemReduced);
	
	if (boost_ >= ignore_deviation_e) boost = ignore_deviation_e;
	elif (boost_ >= consume_less_stamina_e) boost = consume_less_stamina_e;
	elif (boost_ >= immune_to_stun_e) boost = immune_to_stun_e;
	elif (boost_ >= regain_mp_percent_per_tour_e) boost = regain_mp_percent_per_tour_e;
	elif (boost_ >= regain_stamina_per_tour_e) boost = regain_stamina_per_tour_e;
	elif (boost_ >= stamina_immune_to_rain_e) boost = stamina_immune_to_rain_e;
	elif (boost_ >= less_mp_percent_e) boost = less_mp_percent_e;
	elif (boost_ >= inverse_streams_e) boost = inverse_streams_e;
	elif (boost_ >= full_stamina_restore_e) boost = full_stamina_restore_e;
	elif (boost_ >= remove_rain_e) boost = remove_rain_e;
	printf("boost %d, \n", boost);
	
	itemReduced -= boost;
	
	return boost;
};


std::string Item::get_info(int item) {
	std::string name_ = itemsNamesMap.get(item);
	printf("name %s, \n", name_.data());
	return name_;
}

