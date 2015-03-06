#include <algorithm>
#include <iostream>
#include <math.h>
#include "../NR_LIB/code/nr3.h"
#include "../NR_LIB/code/svd.h"
#include <fstream>

using namespace std;

int main() {
    //read data
    ifstream data("../d2");
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
	//Parameters
	SVD result(A);
	// SVD decomposition
//	result = result.range(0.5);

	cout << "Printing W" << endl; result.w.print();
	cout << "Printing V" << endl; result.v.print();

    VecDoub q(4);
    result.solve(z,q);

	cout << "Parameters: "; q.print();
	//Residual error
	double residualError;
	residualError = (A*q-z).length();
	cout << "Residual error: " << residualError << endl;

	//std. deviation
	VecDoub StdDeviation(result.n);
	for(int j = 0; j < result.n; j++) {
		StdDeviation[j] = 0;
        for(int i = 0; i < result.n; i++) {
			StdDeviation[j] += pow(((result.v[j][i])/(result.w[i])),2);
		}
		StdDeviation[j] = sqrt(StdDeviation[j]);
	}
	cout << "standard deviations: "; StdDeviation.print();

    cout << "new stuff\n";
//	MatDoub Wi(4,4);
//	for (int i = 0; i < 4; i++ ){
//		for( int j = 0; j < 4; j++ ){
//			if ( i == j){
//				Wi[i][j]= result.w[j] < 0.5 ? 0 : 1/result.w[j] ;
//			} else {
//				Wi[i][j] = 0;
//			}
//		}
//	}
//	Wi.print();
//	MatDoub newMQ(4,4);
//	VecDoub newQ(4);
//	VecDoub uz(result.u.nrows());
//	uz = result.u.transpose() * z;
//	newMQ = (result.v * Wi);
//	newQ = newMQ * uz;

//	newQ.print();

    //Parameters
    SVD newResult(A);
    // SVD decomposition
//	result = result.range(0.5);

    VecDoub newQ(4);
    newResult.w[3] = 0;

    newResult.solve(z,newQ,1.0);

    cout << "Printing W" << endl; newResult.w.print();
    cout << "Printing V" << endl; newResult.v.print();
    cout << "Parameters: "; newQ.print();

    //Residual error
    double newResidualError;
    newResidualError = (A*newQ-z).length();
    cout << "Residual error: " << newResidualError << endl;

    //std. deviation
    VecDoub newStdDeviation(newResult.n);
    for(int j = 0; j < newResult.n; j++) {
        newStdDeviation[j] = 0;
        for(int i = 0; i < result.n; i++) {
            newStdDeviation[j] += pow(((newResult.v[j][i])/(newResult.w[i])),2);
        }
        newStdDeviation[j] = sqrt(newStdDeviation[j]);
    }
    cout << "standard deviations: "; newStdDeviation.print();


    double newResidual = (A*newQ-z).length();
    cout << newResidual;

	return 0;
}
