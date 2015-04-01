#include <iostream>
#include <cmath>
#include <utility>
#include "../../../NR_LIB/code/nr3.h"
#include "../../../NR_LIB/code/quadrature.h"
#include "../../../NR_LIB/code/svd.h"
//#include "nikolaj.cpp"

double F(double x, double y, double d){
	double f = (0.5*pow(d,2))/(pow(pow(d,2)+pow(x-y,2),(3/2)));
	return f;
}

double G(double x, double y, double h){
	double eps = 0.80;
	double k_1 = (1 - eps);

	return (-k_1*h*F(x,y,1.0));
}

double H(double x, double y, double h){
	double eps = 0.60;
	double k_2 = (1 - eps);

	return (-k_2*h*F(x,y,1.0));
}

VecDoub find_u_and_v(int N){
	double T1=1000, T2 = 500;
	double eps1 = 0.80, eps2=0.60;
	double sigma = 1.712e-9;
	double w=1.0;
	double const_1 = eps1*sigma*pow(T1,4);
	double const_2 = eps2*sigma*pow(T2,4);

	double int_start = -0.5*w;
	double int_end   =  0.5*w;

	double h = (int_end-int_start)/N;

	int size = 2*(N+1);
	MatDoub A(size,size);
	VecDoub b(size);

	for(int row = 0; row < size; row++){
		// fill b
		if(row < (size/2)){
			b[row] = const_1;
		} else {
			b[row] = const_2;
		}

		for(int col = 0; col < size; col++){
			if(col == row){
				// diagonal, = 1
				A[row][col] = 1.0;
			} else if(col >= (size/2) && row < (size/2)){
				// upper right quadrant = G(x,y) or G(x,y)/2
				if((col % (size/2)) == 0 || (col % (size/2)) == (size/2 - 1)){
					// first or last index, = G(x,y)/2
					A[row][col] = G(int_start + (row % (size/2))*h, int_start + (col % (size/2))*h, h)/2;
				} else {
					// not first or last index, = G(x,y)
					A[row][col] = G(int_start + (row % (size/2))*h, int_start + (col % (size/2))*h, h);
				}
			} else if (col < (size/2) && row >= (size/2)){
				// lower left quadrant = H(x,y) or H(x,y)/2
				if((col % (size/2)) == 0 || (col % (size/2)) == ((size/2) - 1)){
					// first or last index, = H(x,y)/2
					A[row][col] = H(int_start + (col % (size/2))*h, int_start + (row % (size/2))*h, h)/2;
				} else {
					// not first or last index, = H(x,y)
					A[row][col] = H(int_start + (col % (size/2))*h, int_start + (row % (size/2))*h, h);
				}
			} else {
				// everything in upper left and lower right quadrants != diagonal, = 0
				A[row][col] = 0.0;
			}
		}
	}

	SVD result(A);
	VecDoub x(size);
	result.solve(b,x);
	double Q_1 = 0, Q_2 = 0;

	for(int a=0; a<size; a++){
		if(a < (size/2)){ //u
			if ((a % (size/2)) == 0 || (a % (size/2)) == (size/2-1)) {
				Q_1 += 0.5*h*((x[a] - const_1)*(-eps1)/(1-eps1) + const_1);
			} else {
				Q_1 += h*((x[a] - const_1)*(-eps1)/(1-eps1) + const_1);
			}
		} else { //v
			if ((a % (size/2)) == 0 || (a % (size/2)) == (size/2-1)) {
				Q_2 += 0.5*h*((x[a] - const_2)*(-eps2)/(1-eps2) + const_2);
			} else {
				Q_2 += h*((x[a] - const_2)*(-eps2)/(1-eps2) + const_2);
			}
		}
	}

	cout << "Q1 and Q2 : " << Q_1 << "\t" << Q_2 << endl;

	return x;
}

int main() {
	for(int i = 4; i <= 4; i*=2){
		VecDoub U_V((i+1)*2);
		U_V = find_u_and_v(i);
		// i and u(x) for -0.5, -0.25, 0, 0.25 and 0.5
		cout << i << "\t"
			 << U_V[0] << "\t"
			 << U_V[i/4] << "\t"
			 << U_V[i/2] << "\t"
			 << U_V[i*3/4] << "\t"
			 << U_V[i] << endl;

	}
	return 0;
}

/*
 *  U:            V:
 *    1398.36       323.266
 * mine:
 *   13821.3        905.071
 * westermann:
 Q1 = 1272.9, Q2 =  -282.54
 */
