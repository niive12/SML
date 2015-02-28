#include <algorithm>
#include <iostream>
#include <math.h>
#include "../NR_LIB/code/nr3.h"
#include "../NR_LIB/code/svd.h"
#include <fstream>

#define OUR 0
#define THEIR 1
#define METHOD THEIR

using namespace std;

int main()
{
	cout << "Find expressions for elements\n";
	cout << "Read files D1, D2, insert in A\n";
	ifstream data("../d1");
	int N;
	N = count(istreambuf_iterator<char>(data), istreambuf_iterator<char>(), '\n');

	VecDoub theta_1(N);
	VecDoub theta_2(N);
	VecDoub x_data(N);
	VecDoub y_data(N);

	if(data.is_open()){
		data.seekg(0,data.beg);
		for(int i = 0; i < N; i++) {
			data >> theta_1[i];
			data >> theta_2[i];
			data >> x_data[i];
			data >> y_data[i];
		}
	} else {
		cout << "File doesn't exist\n";
	}

#if METHOD == OUR
	cout << "Estimate parameters\n";
	// calculate the stuff
    MatDoub A1(N,3),A2(N,3);

    cout << "Number of measurements: " << N << endl;

    // computing a, [[1 cos(theta1) cos(theta1 + theta2)],...]
    for(int i = 0; i < N; i++){
        A1[i][0] = 1;
        A1[i][1] = cos(theta_1[i]);
        A1[i][2] = cos(theta_1[i] + theta_2[i]);

        A2[i][0] = 1;
        A2[i][1] = sin(theta_1[i]);
        A2[i][2] = sin(theta_1[i] + theta_2[i]);
    }

    cout << "A made." << endl;
    // creating two sets of equations, A x_1 = b_1 and A x_2 = b_2
    // where A:[N 3], x_1 and x_2:[3 1] and b_1 and b_2:[N 1]
    SVD result1(A1);
    SVD result2(A2);

    cout << "SVD's' made." << endl;

//    cout << "U1" << endl;
//    //result1.u.print();
//    cout << "W1" << endl;
//    result1.w.print();
//    cout << "V1" << endl;
//    result1.v.print();

//    cout << "U2" << endl;
//    //result2.u.print();
//    cout << "W2" << endl;
//    result2.w.print();
//    cout << "V2" << endl;
//    result2.v.print();


    cout << "result:" << endl;

    VecDoub q1(3),q2(3);

    //result1.solve(x_data,r1);
    result1.solve(x_data,q1);
    result2.solve(y_data,q2);

    q1.print();
    q2.print();


    cout << "Estimate resulting errors\n";


    // std. deviation
    MatDoub stdDeviation(2, result1.n);
    for(int j = 0; j < result1.n; j++)
    {
        stdDeviation[0][j] = 0;
        stdDeviation[1][j] = 0;

        for(int i = 0; i < result1.n; i++)
        {
            stdDeviation[0][j] += pow(((result1.v[j][i])/(result1.w[i])),2);
            stdDeviation[1][j] += pow(((result2.v[j][i])/(result2.w[i])),2);
        }
        stdDeviation[0][j] = sqrt(stdDeviation[0][j]);
        stdDeviation[1][j] = sqrt(stdDeviation[1][j]);
    }

    cout << "\nprinting std. var." << endl;
    stdDeviation.print();

#endif

#if METHOD == THEIR
//    cout << "version 2, one matrix, one equation" << endl;

    MatDoub A(2*N,4);
    VecDoub z(2*N);

    cout << "Number of measurements: " << N << endl;

    // computing a, [[1 cos(theta1) cos(theta1 + theta2)],...]
    for(int i = 0; i < 2*N; i++){
        // x first, then y
        if(i % 2){
            // y, i = odd
            A[i][0] = 0;
            A[i][1] = 1;
            A[i][2] = sin(theta_1[((i - (i % 2))/2)]);
            A[i][3] = sin((theta_1[((i - (i % 2))/2)] + theta_2[((i - (i % 2))/2)]));
            z[i] = y_data[((i - (i % 2))/2)];
        }
        else{
            // x, i = even + {0}
            A[i][0] = 1;
            A[i][1] = 0;
            A[i][2] = cos(theta_1[((i - (i % 2))/2)]);
            A[i][3] = cos((theta_1[((i - (i % 2))/2)] + theta_2[((i - (i % 2))/2)]));
            z[i] = x_data[((i - (i % 2))/2)];
        }
    }

    cout << "A made." << endl;
    //
    SVD result(A);

    cout << "SVD's' made." << endl;

    cout << "U" << endl;
    result.u.print();
    cout << "W" << endl;
    result.w.print();
    cout << "V" << endl;
    result.v.print();


    cout << "result:" << endl;

    VecDoub q(4);

    //result1.solve(x_data,q);
    result.solve(z,q);

    q.print();

    double residualError;

    residualError = (A*q-z).length();
    cout << residualError << endl;


	cout << "Estimate resulting errors\n";


    // std. deviation
    VecDoub stdDeviation(result.n);
    for(int j = 0; j < result.n; j++)
    {
        stdDeviation[j] = 0;
        for(int i = 0; i < result.n; i++)
        {
            stdDeviation[j] += pow(((result.v[j][i])/(result.w[i])),2);
        }
        stdDeviation[j] = sqrt(stdDeviation[j]);
    }

    cout << "\nprinting std. var." << endl;
    stdDeviation.print();

#endif

	return 0;
}


// 4, 6, 50, 40
// 2, 5, 52, 40
