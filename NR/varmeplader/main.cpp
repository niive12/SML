#include <iostream>
#include <cmath>
#include <utility>
#include "../../../NR_LIB/code/nr3.h"
#include "../../../NR_LIB/code/quadrature.h"
#include "../../../NR_LIB/code/svd.h"

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

VecDoub find_u_and_v(int N, double &Q1, double &Q2){
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
	Q1 = Q_1;
	Q2 = Q_2;

	return x;
}
/**
 * Prints the three tables in the report
 * Formattet for latex.
 */
int main() {
	int max = 256;
	int width = 8;
	double Q1_h3 = 0, Q2_h3 = 0;

	cout << fixed << setprecision(width-5);

	cout << setw(3) << "N"       << "&\t"
		 << setw(width) << "u(-1/2)" << "&\t"
		 << setw(width) << "u(-1/4)" << "&\t"
		 << setw(width) << "u(0)"    << "&\t"
		 << setw(width) << "u(1/4)"  << "&\t"
		 << setw(width) << "u(1/2)"  << "\\\\ \\hline" << endl;

	for(int n = 1; n <= max; n*=2){
		VecDoub U_V((n+1)*2);
		U_V = find_u_and_v(n, Q1_h3, Q2_h3);

		if(n >= 4 ){
			cout << setw(3)<< n << "&\t"
				 << setw(width) << U_V[0] << "&\t"
				 << setw(width) << U_V[n/4] << "&\t"
				 << setw(width) << U_V[n/2] << "&\t"
				 << setw(width) << U_V[n*3/4] << "&\t"
				 << setw(width) << U_V[n] << "\\\\ \\hline" << endl;
		}
	}

	cout << "\n\n\n" << setw(3) << "N"       << "&\t"
		 << setw(width) << "v(-1/2)" << "&\t"
		 << setw(width) << "v(-1/4)" << "&\t"
		 << setw(width) << "v(0)"    << "&\t"
		 << setw(width) << "v(1/4)"  << "&\t"
		 << setw(width) << "v(1/2)"  << "\\\\ \\hline" << endl;

	for(int n = 1; n <= max; n*=2){
		VecDoub U_V((n+1)*2);
		U_V = find_u_and_v(n, Q1_h3, Q2_h3);

		if(n >= 4 ){
			cout << setw(3)<< n << "&\t"
				 << setw(width) << U_V[n+1] << "&\t"
				 << setw(width) << U_V[n+1+n/4] << "&\t"
				 << setw(width) << U_V[n+1+n/2] << "&\t"
				 << setw(width) << U_V[n+1+n*3/4] << "&\t"
				 << setw(width) << U_V[2*n+1] << "\\\\ \\hline" << endl;
		}
	}

	double Q1_h1 = 0, Q1_h2 = 0, Q2_h1 = 0, Q2_h2 = 0;
	double error1, error2, alpha_k = 2;

	width = 15;
	cout << fixed << setprecision(width-5);

	cout << "\n\n\n"<< setw(3)       << "n" << "&\t"
		 << setw(width) << "Q1" << "&\t"
		 << setw(width) << "Q2" << "&\t"
		 << setw(width) << "error1" << "&\t"
		 << setw(width) << "error2" << "\\\\ \hline"
		 << endl;

	for(int n = 1; n <= max; n*=2){
		VecDoub U_V((n+1)*2);
		Q1_h1 = Q1_h2;
		Q1_h2 = Q1_h3;
		Q2_h1 = Q2_h2;
		Q2_h2 = Q2_h3;
		U_V = find_u_and_v(n, Q1_h3, Q2_h3);
//		alpha_k = (Q1_h1-Q1_h2)/(Q1_h2-Q1_h3);
//		alpha_k = (Q2_h1-Q2_h2)/(Q2_h2-Q2_h3);
		alpha_k = 4;
		error1 = (Q1_h2-Q1_h1)/(alpha_k-1);
		error2 = (Q2_h2-Q2_h1)/(alpha_k-1);

		if ( n >= 4 ) {
			cout << setw(3)     << n << "&\t"
				 << setw(width) << Q1_h3 << "&\t"
				 << setw(width) << Q2_h3 << "&\t"
				 << setw(width) << error1 << "&\t"
				 << setw(width) << error2 << "\\\\ \hline"
				 << endl;
		}
	}
	return 0;
}
