#include "../../../NR_LIB/code/ludcmp.h"

double func_F(double x, double y, double d = 1.0){
	return 0.5* (pow(d,2)/pow((pow(d,2)+pow((x-y),2)),3/2));
}

VecDoub niko_find_u_and_v(int N){
	double T1=1000, T2 = 500;
	double eps1 = 0.80, eps2=0.60;
	double sigma = 1.712e-9;
	double w=1.0;
	double const_1 = eps1*sigma*pow(T1,4);
	double const_2 = eps2*sigma*pow(T2,4);

	//	cout << const_1 << "\t" << const_2 << "\t";

	double int_start = -0.5*w;
	double int_end   =  0.5*w;

	double h = (int_end-int_start)/N;

	int size = 2*(N+1);
	MatDoub A(size,size);
	VecDoub b(size);

	VecDoub parts(N+1);
	for( int i = 0; i < N+1; i++){
		parts[i] = (i*h)*(int_end-int_start)+int_start;
		//        Khan Acadamy har forklaret dette meget fint... youtube.com/watch?v=h3h--K5928M
	}

	int xx,yy;
	for(int aa=0; aa<size; aa++){
		for(int bb=0; bb<size; bb++){
			if (aa == bb ){
				A[aa][bb] = 1;
			} else {
				if(!(aa%2)){ //even row... y changes (u)
					xx = aa/2;
					yy = bb/2;
					b[aa] = const_1;
					if((bb % 2)){
						if (yy == 0 || yy == N) {
							A[aa][bb] = -(1-eps1) * 0.5 * h * func_F(parts[xx],parts[yy]);
						} else {
							A[aa][bb] = -(1-eps1) * h * func_F(parts[xx],parts[yy]);
						}
					} else {
						A[aa][bb] = 0;
					}
				} else { //odd row, x changes (v)
					yy = aa/2;
					xx = bb/2;
					b[aa] = const_2;
					if(!(bb % 2)){
						if(xx == 0 || xx == N){
							A[aa][bb] = -(1-eps2) * 0.5 * h * func_F(parts[xx],parts[yy]);
						} else {
							A[aa][bb] = -(1-eps2) * h * func_F(parts[xx],parts[yy]);
						}
					} else {
						A[aa][bb] = 0;
					}
				}
			}
		}
	}
	A.print();
	LUdcmp result(A);
	VecDoub x(size);
	result.solve(b,x);
	x.print();
	double Q_1 = 0, Q_2 = 0;

	for(int a=0; a<size; a++){
		if( !(a % 2) ){ //u
			if ( a == 0 || a == size-2 ) {
				Q_1 += 0.5*h*((x[a] - const_1)*(-eps1)/(1-eps1) + const_1);
			} else {
				Q_1 += h*((x[a] - const_1)*(-eps1)/(1-eps1) + const_1);
			}
		} else { //v
			if ( a == 1 || a == size-1 ) {
				Q_2 += 0.5*h*((x[a] - const_2)*(-eps2)/(1-eps2) + const_2);
			} else {
				Q_2 += h*((x[a] - const_2)*(-eps2)/(1-eps2) + const_2);
			}
		}
	}

	cout << "Q1 and Q2 : " << Q_1 << "\t" << Q_2 << endl;

	return x;
}
