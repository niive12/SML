#include <iostream>
#include <math.h>
#include "../NR_LIB/code/nr3.h"
#include "../NR_LIB/code/svd.h"
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

    // calculate the stuff
    int N = theta_1.length();
    MatDoub A(N,3);

    cout << "Number of measurements: " << N << endl;

    // computing a, [[1 cos(theta1) cos(theta1 + theta2)],...]
    for(int i = 0; i < N; i++){
        A[i][1] = 1;
        A[i][2] = cos(theta_1[i]);
        A[i][3] = cos(theta_1[i] + theta_2[i]);
    }

    // creating two sets of equations, A x_1 = b_1 and A x_2 = b_2
    // where A:[N 3], x_1 and x_2:[3 1] and b_1 and b_2:[N 1]
    SVD result1(A);
    SVD result2(A);

    result1.u.print();
    result1.w.print();
    result1.v.print();

    result2.u.print();
    result2.w.print();
    result2.v.print();

	cout << "Estimate resulting errors\n";








	return 0;
}

