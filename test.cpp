#include <iostream>
// ./minic++ test.cpp 1> /tmp/mips1.s && echo "-=-=-=-=-" && cat /tmp/mips1.s && echo "-=-=-=-=-" && mars /tmp/mips1.s

class A {
	public:
	int x;
	int getX();
	/*int getX2();
	void setX(int x);
	void setX2(int y);*/
	A(int x);
};

/*
int fun (int x, int y) {
	int z = x - y;
	return z;
}

void swap (int *x, int *y) {
	int t = *x;
	*x = *y;
	*y = t;
	return;
}
*/
A::A(int z) {
	x = z;
	/*x = 2;
	int x = 4;
	this->getX();*/
}

int A::getX() {
	return this->x;
}
/*
int A::getX2() {
	return x;
}

void A::setX(int x) {
	this->x = x;
}

void A::setX2(int y) {
	x = y;
	x = 2;
}


int &ref (int *i) {
	return *i;
}
*/

int main() {
	
	A a = A(4);
	std::cout << a.x << "\n";
	
	//std::cout << "\x1b[32;01m*Vert*\x1b[0m\n"; // affiche rien…
	/*
	std::cout << "\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\n";
	std::cout << "\x30\x31\x32\x33\x34\x35\x36\x37\x38\x39\x3a\x3b\x3c\x3d\x3e\x3f\n";
	std::cout << "\x40\x41\x42\x43\x44\x45\x46\x47\x48\x49\x4a\x4b\x4c\x4d\x4e\x4f\n";
	std::cout << "\x50\x51\x52\x53\x54\x55\x56\x57\x58\x59\x5a\x5b\x5c\x5d\x5e\x5f\n";
	std::cout << "\x60\x61\x62\x63\x64\x65\x66\x67\x68\x69\x6a\x6b\x6c\x6d\x6e\x6f\n";
	std::cout << "\x70\x71\x72\x73\x74\x75\x76\x77\x78\x79\x7a\x7b\x7c\x7d\x7e\x7f\n";
	*/
	
	std::cout << "Done.\n";
	return 0;
}





