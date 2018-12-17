

// Client side C/C++ program to demonstrate Socket programming


#include <functional>
#include <thread>
#include <chrono>

#include <cstdlib>
#include <cstdio>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <zconf.h>
#include <unistd.h>

#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>


#define elif else if

#define SKEY int(1e8)
#define PORT 50006
#define NCELS 100*100
#define fori(s, e) for (int i=s; i<e; i++)


#define INFO_ID_HASH 0
#define INFO_ID_SIZE 1
#define INFO_ID_CHECK 2

#define elif else if


//#define DEBUG
inline int sleep(int millis){
	std::this_thread::sleep_for(std::chrono::milliseconds(millis));
}

template<unsigned s, unsigned N>
int hash_arr(const int arr[N + 2 * s]) {
	ssize_t seed = N;
	
	fori(s, N + s) {
		seed ^= arr[i] + 0x9e3779b9 + (seed >> 2) + (seed << 1);
	}
	
	
	return (int) seed;
}


template<unsigned n>
void ppr_info(int info[n], ssize_t nread, char opt) {
	static int cnt = 0;
	
	
	
	if (opt == 'i') printf("[%d] IN <<< ", cnt++);
	elif (opt == 'o') printf("[%d] OUT >>> ", cnt++);
	
	printf("info(len:%d, hash: %d, verify[%d]), nbytes: %zu\n",
	       info[INFO_ID_SIZE], info[INFO_ID_HASH], info[INFO_ID_CHECK], nread);
	
	
}

template<unsigned n>
bool check_info(const int infoIN[n], int infoOUT[n]) {
	
	infoOUT[INFO_ID_CHECK] = infoIN[INFO_ID_CHECK] == 1;
	
	for (int i = 0; i < n; i++) infoOUT[INFO_ID_CHECK] &= (infoIN[i] == infoOUT[i]);
	
	return infoOUT[INFO_ID_CHECK] == 1;
}


template<unsigned n, unsigned N>
bool check_packet(const int arr[N + 2], int infoIN[n], int infoOUT[n]) {
	
	int lenIn = infoIN[INFO_ID_SIZE];
	int hashIn = infoIN[INFO_ID_HASH];
	
	int hashArr = (int) hash_arr<1, N>(arr);
	
	bool lensCheck = (lenIn == N);
	
	infoOUT[INFO_ID_HASH] = hashArr;
	
	bool hashCheck = (hashArr == hashIn) &&
	                 (hashIn == arr[0]) &&
	                 (hashIn == arr[N + 1]);
	
	#ifdef DEBUG
	printf("[%d, %d] : [%d==%d == %d==%d]\n",
	       lensCheck, hashCheck,
	       hashIn, hashArr, arr[0], arr[N + 1]
	);
	
	#endif
	
	infoOUT[INFO_ID_CHECK] = lensCheck && hashCheck;
	
	return infoOUT[INFO_ID_CHECK] == 1;
	
}

template<unsigned n, unsigned N>
int srv_prepare_packet(int infoOUT[n], int arr[N + 2]) {
	
	arr[0] = 0;
	arr[N + 1] = 0;
	
	int hash = (int) hash_arr<1, N>(arr);
	
	infoOUT[INFO_ID_SIZE] = N;
	infoOUT[INFO_ID_HASH] = hash;
	infoOUT[INFO_ID_CHECK] = 1;
	
	arr[0] = hash;
	arr[N + 1] = hash;
	
	return 0;
	
	
}

template<unsigned n>
int srv_init_negotiation(int sock, int infoIN[n], int infoOUT[n]) {
	//1 - get static and dynamic key in infoIN
	ssize_t nread, nsend;
	
	//2 - send it back to verify we have the same infoIN and server
	nsend = send(sock, infoOUT, sizeof(int) * n, 0);
	#ifdef DEBUG
	ppr_info<n>(infoOUT, nsend, 'o');
	#endif
	nread = read(sock, infoIN, sizeof(int) * n);
	#ifdef DEBUG
	ppr_info<n>(infoIN, nread, 'i');
	#endif
	return 0;
};

template<unsigned n>
int cli_init_negotiation(int sock, int *infoIN, int *infoOUT) {
	//1 - get static and dynamic key in infoIN
	ssize_t nread, nsend;
	
	nread = read(sock, infoIN, sizeof(int) * n);
	#ifdef DEBUG
	ppr_info<n>(infoIN, nread, 'i');
	#endif
	fori(0, n) infoOUT[i] = infoIN[i];
	
	//2 - send it back to verify we have the same infoIN and server
	nsend = send(sock, infoOUT, sizeof(int) * n, 0);
	#ifdef DEBUG
	ppr_info<n>(infoOUT, nsend, 'o');
	#endif
	return 1;
};


template<unsigned n>
bool srv_verify(int sock, int infoIN[n], int infoOUT[n]) {
	//5 - read confirmation
	ssize_t nread, nsend;
	
	check_info<n>(infoIN, infoOUT);
	
	nsend = send(sock, infoOUT, sizeof(int) * n, 0);
	#ifdef DEBUG
	ppr_info<n>(infoOUT, nsend, 'o');
	#endif
	nread = read(sock, infoIN, sizeof(int) * n);
	#ifdef DEBUG
	ppr_info<n>(infoIN, nread, 'i');
	#endif
	//6 - verify its true and check if info not changed, then send back
	check_info<n>(infoIN, infoOUT);
	
	return infoOUT[INFO_ID_CHECK] != 0;
	
};

template<unsigned n>
bool cli_confirm_verification(int sock, int infoIN[n], int infoOUT[n]) {
	//5 - read confirmation
	ssize_t nread, nsend;
	nread = read(sock, infoIN, sizeof(int) * n);
	#ifdef DEBUG
	ppr_info<n>(infoIN, nread, 'i');
	#endif
	//6 - verify its true and check if info not changed, then send back
	check_info<n>(infoIN, infoOUT);
	
	nsend = send(sock, infoOUT, sizeof(int) * n, 0);
	#ifdef DEBUG
	ppr_info<n>(infoOUT, nsend, 'o');
	#endif
	return infoOUT[INFO_ID_CHECK] != 0;
	
};

template<unsigned n, unsigned N>
bool srv_send_array(int sock, int infoIN[n], int infoOUT[n], int arr[N + 2]) {
	
	ssize_t nread, nsend;
	
	
	bool candosend = infoOUT[INFO_ID_CHECK] != 0;
	
	if (candosend) {
		
		nsend = send(sock, arr, sizeof(int) * (N + 2), 0);
		//#ifdef DEBUG
		if (nsend != sizeof(int) * (N + 2)) ppr_info<n>(infoOUT, nsend, 'o');
		//#endif
		nread = read(sock, infoIN, sizeof(int) * n);
		
		check_info<n>(infoIN, infoOUT);
		#ifdef DEBUG
		ppr_info<n>(infoIN, nread, 'i');
		#endif
		return infoIN[INFO_ID_CHECK] == 1;
		
	} else {
		return false;
	}
	
	
}

template<unsigned n, unsigned N>
bool cli_receive_array(int sock, int infoIN[n], int infoOUT[n], int arr[N]) {
	
	ssize_t nread, nsend;
	
	bool candorecv = infoIN[INFO_ID_CHECK] != 0;
	const int hash = infoIN[INFO_ID_HASH];
	
	
	if (candorecv) {
		
		nread = read(sock, arr, sizeof(int) * (N + 2));
		
		
		if (nread != sizeof(int) * (N + 2)) ppr_info<n>(infoOUT, nread, 'i');
		
		check_packet<n, N>(arr, infoIN, infoOUT);
		
		
		
		#ifdef DEBUG
		ppr_info<n>(infoIN, nread, 'i');
		for (int j = 0; j < 4; j++) printf("...%d: %d\n", j, arr[j]);
		for (int j = N - 4; j < N + 2; j++) printf("...%d: %d\n", j, arr[j]);
		#endif
		
		nsend = send(sock, infoOUT, sizeof(int) * n, 0);
		#ifdef DEBUG
		ppr_info<n>(infoOUT, nread, 'o');
		#endif
		return infoOUT[INFO_ID_CHECK] == 1;
		
	} else {
		return false;
	}
	
	
}


int init_connection(int sock, int infoIN[], int infoOUT[]);

int confirm_verification(int sock, int infoIN[], int infoOUT[]);

int verify_and_recv_arr(int sock, int infoIN[], int infoOUT[]);

int main_cli() {
	
	
	printf("RUN CLIENT...\n");
	
	
	struct sockaddr_in address;
	int sock = 0;
	ssize_t nread;
	struct sockaddr_in serv_addr;
	char *hello = "Hello from client";
	char buffer[1024] = {0};
	int bufferi[NCELS + 2];
	
	if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		printf("\n Socket creation error \n");
		return -1;
	}
	
	
	memset(&serv_addr, '0', sizeof(serv_addr));
	
	serv_addr.sin_family = AF_INET;
	serv_addr.sin_port = htons(PORT);
	
	// Convert IPv4 and IPv6 addresses from text to binary form
	if (inet_pton(AF_INET, "127.0.0.1", &serv_addr.sin_addr) <= 0) {
		printf("\nInvalid address/ Address not supported \n");
		return -1;
	}
	
	if (connect(sock, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0) {
		printf("\nConnection Failed \n");
		return -1;
	}
	send(sock, hello, strlen(hello), 0);
	printf("Hello message sent\n");
	nread = read(sock, buffer, 1024);
	printf("%s\n", buffer);
	int infoIN[3];
	int infoOUT[3];
	int i = 0;
	
	for (int j = 0; j < 1e6; j++) {
		bool doWhileVerified = false;
		i = 0;
		while (!doWhileVerified && i++ < 100) {
			
			cli_init_negotiation<3>(sock, infoIN, infoOUT);
			cli_confirm_verification<3>(sock, infoIN, infoOUT);
			doWhileVerified = cli_receive_array<3, NCELS>(sock, infoIN, infoOUT, bufferi);
			
		}
	}
	
	return 0;
}


int main_srv() {
	
	printf("RUN SERVER...\n");
	
	int server_fd, new_socket;
	ssize_t valread;
	struct sockaddr_in address{};
	int opt = 0;
	int addrlen = sizeof(address);
	char buffer[1024] = {0};
	char *hello = "Hello from server";
	int bufferi[NCELS + 2] = {0};
	
	// Creating socket file descriptor
	if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) == 0) {
		perror("socket failed");
		exit(EXIT_FAILURE);
	}
	
	// Forcefully attaching socket to the port 8080
	if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR | SO_REUSEPORT,
	               &opt, sizeof(opt))) {
		perror("setsockopt");
		exit(EXIT_FAILURE);
	}
	address.sin_family = AF_INET;
	address.sin_addr.s_addr = INADDR_ANY;
	address.sin_port = htons(PORT);
	
	// Forcefully attaching socket to the port 8080
	if (bind(server_fd, (struct sockaddr *) &address,
	         sizeof(address)) < 0) {
		perror("bind failed");
		exit(EXIT_FAILURE);
	}
	if (listen(server_fd, 3) < 0) {
		perror("listen");
		exit(EXIT_FAILURE);
	}
	if ((new_socket = accept(server_fd, (struct sockaddr *) &address,
	                         (socklen_t *) &addrlen)) < 0) {
		perror("accept");
		exit(EXIT_FAILURE);
	}
	
	valread = read(new_socket, buffer, 1024);
	printf("%s\n", buffer);
	
	send(new_socket, hello, strlen(hello), 0);
	printf("Hello message sent\n");
	
	for (int i = 1; i < NCELS + 1; i++) {
		bufferi[i] = (i - 1);
	}
	
	
	int len = sizeof(bufferi) / sizeof(int);
	
	
	int i = 0;
	int infoIN[3];
	int infoOUT[3];
	

	int sum = 0;
	int cntmax = 0;
	for (int j = 0; j < 1e6; j++) {
		bool doWhileVerified = false;
		i = 0;
		while (!doWhileVerified) {
			
			//0 - send static and dynamic key in infoIN
			
			srv_prepare_packet<3, NCELS>(infoOUT, bufferi);
			srv_init_negotiation<3>(new_socket, infoIN, infoOUT);
			doWhileVerified = srv_verify<3>(new_socket, infoIN, infoOUT);
			
			if (!doWhileVerified) continue;
			
			doWhileVerified = srv_send_array<3, NCELS>(new_socket, infoIN, infoOUT, bufferi);
			
			if (doWhileVerified == false) sleep(1);
			
		}
		
		if(i > cntmax) cntmax = i;
		if (i > 98) printf("i=%d\n",i);
		
		sum += i;
		
		if (j%1000 == 0) printf("...[%d] : cntmax=%d, sum=%d, sum=%f\n",j,cntmax,sum,sum/1000.0);
		
	}
	
	printf("sum=%d, sum=%f\n",sum,sum/1e6);
	
	close(new_socket);
	
	return 0;
}

int main(int argc, char *argv[]) {
	
	int opt = 0;
	if (argc == 2) opt = std::stoi(argv[1]);
	
	if (opt == 0) main_srv();
	elif (opt == 1) main_cli();
	else return -1;
	
	return 0;
	
}

