#include <iostream>
#include <cmath>
#include <utility>
#include "../../../NR_LIB/code/nr3.h"
#include "../../../NR_LIB/code/quadrature.h"
//#include "../../../nr_robot/trivial_functions.h"
//#include "../../../NR_LIB/code/ludcmp.h"
#include "../../../NR_LIB/code/svd.h"

//double func_F_x(double x){
//    const double d = 1.0;
//    double y = 0.25;
//    return 0.5* (pow(d,2)/pow((pow(d,2)+pow(x-y,2)),3/2)) * x;
//}

//double func_F_y(double y){
//    const double d = 1.0;
//    double x = 0.25;
//    return 0.5* (pow(d,2)/pow((pow(d,2)+pow(x-y,2)),3/2))*y;
//}

//template<class T>
//double int_func(T &funcc, double aa, double bb, int N = 4){
////	const double min_error = 10e-6;
//    int max_iter = N;
//    double s_h1 = 1, s_h2 = 1, s_h3 = 1;//, error,alpha_k;

//    Trapzd<T> int_trapez(funcc, aa, bb);
//    for(int iterations = 0; iterations < max_iter; iterations++){
//        s_h1 = s_h2;
//        s_h2 = s_h3;
//        s_h3 = int_trapez.next();
////		alpha_k = (s_h1-s_h2)/(s_h2-s_h3);
////		error = (s_h2-s_h1)/(alpha_k-1);
////		if(error < min_error){
////			return s_h3;
////		}
//    }
//    return s_h3;
//}

//double func_u(double v_0, int N){
//    int T=1000;
//    double eps = 0.80;
//    double sigma = 1.712e-9;
//    double w=1.0;

//    double I = int_func(func_F_x,-0.5*w, 0.5*w, N) * v_0;
//    return eps*sigma*pow(T,4)+(1-eps)*I;
//}

//double func_v(double u_0, int N){
//    int T=500;
//    double eps = 0.60;
//    double sigma = 1.712e-9;
//    double w=1.0;

//    double I = int_func(func_F_x,-0.5*w, 0.5*w, N) * u_0;
//    return eps*sigma*pow(T,4)+(1-eps)*I;
//}

// lukas funcs
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
// lukas funcs end

//double func_Q1(double u_0){
//    double w = 1.0;
//    double I = int_func(func_F_x,-0.5*w, 0.5*w);
//    return (u_0 - I);
//}
//double func_Q2(double v_0){
//    double w = 1.0;
//    double I = int_func(func_F_y,-0.5*w, 0.5*w);
//    return (v_0 - I);
//}

//int main() {
//	double u_0=0,v_0=0,u_1,v_1;
//	int N = 4;
//	double w = 10.0;
//	double q1,q2;
//	int tN = 32;
//	for( int d=2; d <= tN; d=d*2){
//		cout << d << endl;
//		for( int i = 0; i < N; i++){
//			u_1 = func_u(v_0, d);
//			v_1 = func_v(u_0, d);
//			u_0 = u_1;
//			v_0 = v_1;

//			q1 = int_func(func_Q1, -0.5*w,0.5*w,d);
//			q2 = int_func(func_Q2, -0.5*w,0.5*w,d);

//			cout << "q1 = " << q1 << "\tq2 = " << q2 << endl;
//			cout << "u = " << u_0 << "\tv = " << v_0 << endl;
//		}
//		u_0 = 0;
//		v_0 = 0;
//	}
//	return 0;
//}

//double func_F(double x, double y, double d = 1.0){
//    return 0.5* (pow(d,2)/pow((pow(d,2)+pow((x-y),2)),3/2));
//}

VecDoub find_u_and_v(int N){
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

    //	VecDoub parts(N+2);
    //	for( int i = 0; i < N+2; i++){
    //		parts[i] = int_end - (int_end + (double((2*i)-(N+1))/(N+1) * int_start));
    //	}
    //    VecDoub parts(N+1);
    //    for( int i = 0; i < N+1; i++){
    //        parts[i] = (i*h)*(int_end-int_start)+int_start;
    ////        Khan Acadamy har forklaret dette meget fint... youtube.com/watch?v=h3h--K5928M
    //    }

    //	I = int_func(func_F_x,-0.5*w, 0.5*w, N) * v_0;
    //	u(x) = const_1 + (1-eps)*I;
    //	u(x) - (1-eps)*I = const_1;


    int size = 2*(N+1);
    MatDoub A(size,size);
    VecDoub b(size);

    // lukas A
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

//    b.print();
//    A.print();
    // lukas A end

    //    int x,y;
    //    for(int a=0; a<size; a++){
    //        for(int b=0; b<size; b++){
    //            if (a == b ){
    //                A[a][b] = 1;
    //            } else {
    //                if(!(a%2)){ //even row... y changes (u)
    //                    x = a/2;
    //                    y = b/2;
    //                    z[a] = const_1;
    //                    if((b % 2)){
    //                        if (y == 0 || y == N) {
    //                            A[a][b] = (1-eps1) * 0.5 * h * func_F(parts[x],parts[y]);
    //                        } else {
    //                            A[a][b] = (1-eps1) * h * func_F(parts[x],parts[y]);
    //                        }
    //                    } else {
    //                        A[a][b] = 0;
    //                    }
    //                } else { //odd row, x changes (v)
    //                    y = a/2;
    //                    x = b/2;
    //                    z[a] = const_2;
    //                    if(!(b % 2)){
    //                        if(x == 0 || x == N){
    //                            A[a][b] = (1-eps2) * 0.5 * h * func_F(parts[x],parts[y]);
    //                        } else {
    //                            A[a][b] = (1-eps2) * h * func_F(parts[x],parts[y]);
    //                        }
    //                        x++;
    //                    } else {
    //                        A[a][b] = 0;
    //                    }
    //                }
    //            }
    //        }
    //    }


    SVD result(A);
    VecDoub x(size);
    result.solve(b,x);
    //	u_and_v.print();
    //    pair<double,double> U_V(u_and_v[(size-1)/2],u_and_v[(size-1)/2+1]);
    //    pair<VecDoub,VecDoub> U_V(size,size);

    //	double I_1 = 0, I_2 = 0, line;
    //	for(int a=0; a<size; a++){
    //		for(int b=a+1; b<size; b++){
    //			line = A[a][b];
    //			if(!(a%2)){
    ////				line *= u_and_v[a/2];
    //				I_1 += line;
    //			} else {
    ////				line *= u_and_v[a/2+1];
    //				I_2 += line;
    //			}
    //		}
    //	}

//        double Q_1 = x[0] - const_1/(1-eps1), Q_2 = x[1] - const_2/(1-eps2);
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
 Q1 = 1272.9, Q2 =  282.54
 */
