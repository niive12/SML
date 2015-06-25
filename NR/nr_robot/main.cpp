#include <algorithm>
#include <iostream>
#include <cmath>
#include "../NR_LIB/code/nr3.h"
#include "../NR_LIB/code/svd.h"
#include <fstream>
//#include "trivial_functions.h" //length and print functions

using namespace std;

void analyze(char* datafile, double threshold = -1){
	cout << datafile;
	if(threshold > 0 ){
		cout << " With threshold: " << setprecision(2) << threshold;
	}
	cout << endl;
//	Load file
	ifstream data(datafile);
	int N;
	N = count(istreambuf_iterator<char>(data), istreambuf_iterator<char>(), '\n');
	VecDoub theta_1(N), theta_2(N), x_data(N), y_data(N);
	if( data.is_open() ) {
		data.seekg(0,data.beg);
		for(int i = 0; i < N; i++) {
			data >> theta_1[i];
			data >> theta_2[i];
			data >> x_data[i];
			data >> y_data[i];
		}
	} else throw("File doesn't exist\n");

//	Load into matrix
	MatDoub A(2*N,4);
	VecDoub z(2*N);
	// computing a, [[1 cos(theta1) cos(theta1 + theta2)],...]
	for(int i = 0; i < 2*N; i++) {
		// x first, then y
		if(i % 2) { // y, i = odd
			A[i][0] = 0;
			A[i][1] = 1;
			A[i][2] = sin(theta_1[((i-1)/2)]);
			A[i][3] = sin((theta_1[((i-1)/2)] + theta_2[((i-1)/2)]));
			z[i] = y_data[((i-1)/2)];
		} else { // x, i = even + {0}
			A[i][0] = 1;
			A[i][1] = 0;
			A[i][2] = cos(theta_1[(i/2)]);
			A[i][3] = cos((theta_1[(i/2)] + theta_2[(i/2)]));
			z[i] = x_data[(i/2)];
		}
	}

//	SVD decomposition
	SVD result(A);

	cout << "Printing W" << endl; result.w.print();
	cout << "Printing V" << endl; result.v.print();

	VecDoub q(4);
	result.solve(z,q,threshold);

	cout << "Parameters: "; q.print();
//	Residual error
	double residualError;
	residualError = (A*q-z).length();
	cout << "Residual error: " << residualError << endl;

//	std. deviation
	VecDoub StdDeviation(result.n);
	for(int j = 0; j < result.n; j++) {
		StdDeviation[j] = 0;
		for(int i = 0; i < result.n; i++) {
			if(result.w[i] <= threshold) {
				result.v[j][i] = 0;
			}
			StdDeviation[j] += pow(((result.v[j][i])/(result.w[i])),2);
		}
		StdDeviation[j] = sqrt(StdDeviation[j]);
	}
	cout << "standard deviations: "; StdDeviation.print();
}

int main() {

	analyze("../d1");
	analyze("../d2");
	analyze("../d2",1.0);

	return 0;
}
