#include <iostream>
#include "../NR_LIB/code/nr3.h"
#include <fstream>

using namespace std;

int main()
{
	cout << "Find expressions for elements\n";
	cout << "Read files D1, D2, insert in A\n";
	int N = 500;

	VecDoub theta_1(N);
	VecDoub theta_2(N);
	VecDoub x_data(N);
	VecDoub y_data(N);
	//layout: theta_1^{(1)}    theta_2^{(1)}   x^{(1)}    y^{(1)}
	ifstream data("../d1");
	if(data.is_open()){
		for(int i = 0; i < 500; i++) {
			data >> theta_1[i];
			data >> theta_2[i];
			data >> x_data[i];
			data >> y_data[i];
		}
	} else {
		cout << "File doesn't exist\n";
	}
	cout << "Estimate parameters\n";
	cout << "Estimate resulting errors\n";
	return 0;
}

