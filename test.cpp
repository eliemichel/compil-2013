#include <iostream>
// ./minic++ test.cpp 1> /tmp/mips1.s && echo "-=-=-=-=-" && cat /tmp/mips1.s && echo "-=-=-=-=-" && mars /tmp/mips1.s

int i, j;

int fun (int x, int y) {
	int z = x - y;
	return z;
}

int main() {
	std::cout << "\nThe answer " << "is ";
	std::cout << (i = ((43 - 1)  + 360 * 27) % 360) << " - " << i;
	i = 5 + 2 * 42;
	std::cout << "..." << i - 47;
	if (i < 0)
		std::cout << " i est strictement negatif ";
	else
		std::cout << " i est positif ";
	
	int k = 5;
	if (true) {
		k = 2;
		std::cout << "| k = " << k << " ";
		int k = 10;
		std::cout << "| k = " << k << " ";
	}
	std::cout << "| k = " << k << " ";
	
	i = 0;
	while (i < 10) {
		std::cout << " i = " << i << "   ";
		i = i + 1;
	}
	
	i = 0;
	int j = 4;
	std::cout << "| fun (j) = " << fun (j, 1);
	
	return 0;
}





