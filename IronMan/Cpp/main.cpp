#include <iostream>

#include "objects.h"

int main(){
	
	printf("removecloud = %d\n",removecloud_e);
	printf("restorestamina = %d\n",restorestamina_e);
	printf("invertstreams = %d\n",invertstreams_e);
	printf("shoes = %d\n",shoes_e);
	printf("flippers = %d\n",flippers_e);
	
	printf("cycletire = %d\n",cycletire_e);
	printf("umbrella = %d\n",umbrella_e);
	printf("energyboots = %d\n",energyboots_e);
	printf("potion = %d\n",potion_e);
	printf("helmet = %d\n",helmet_e);
	
	printf("staminasale = %d\n",staminasale_e);
	printf("spikeshoes = %d\n",spikeshoes_e);
	printf("cyklop = %d\n",cyklop_e);
	printf("bicyclehandlebar = %d\n",bicyclehandlebar_e);
	
	Item removecloud = Item(removecloud_e);
	
	printf("%d, %d, %d\n",removecloud.item, removecloud.itemType, removecloud.effect);
	
	return 0;
}
