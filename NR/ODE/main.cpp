#include <iostream>
#include <iomanip>
#include <cmath>
#include <vector>

#define EULER 0
#define LEAP_FROG 1

using namespace std;

vector<double> y_marks(vector<double> prev){
    vector<double> res(2);

    res[0] = prev[0]*prev[1];
    res[1] = -pow(prev[0],2);

    return res;
}


template <class T>
vector<double> ode_method(T &func, vector<double> init, int N, double h, int method){

    vector<double> x_vals_prev = init;
    vector<double> x_vals_new = init;

    for(int n = 0; n < N; n++){
        for(int i = 0; i < 2; i++){
            if(method == EULER){
                double mark = func(x_vals_prev)[i];
                x_vals_new[i] = x_vals_prev[i] + h*(mark);
            } else if(method = LEAP_FROG){

            }
        }
        x_vals_prev = x_vals_new;
    }

    return x_vals_new;
}


int main()
{
    // make eulers!
    // output h, y_0^h(x),
    cout << "Hello Euler!" << endl;


    vector<double> initials(2);
    initials[0] = 1;
    initials[1] = 1;

    vector<double> result_h1(2);
    vector<double> result_h2(0,2);
    vector<double> result_h3(0,2);


    double X = 10;

    cout << "For X = " << X << endl;

    int width = 20;
    cout << fixed << setprecision(width-5);

    cout << setw(width) << "h\t"
         << setw(width) << "y[0]\t"
         << setw(width) << "error y[0]\t"
         << setw(width) << "y[1]\t"
         << setw(width) << "error y[1]\t"
         << setw(width) << "y[0]^2 + y[1]^2"
         << endl;



    for(int N = 50; N <= 1280*8; N = N*2){
        double h = X/double(N);

        result_h1 = ode_method(y_marks, initials, N, h, EULER);

        if(N >= 200)
            cout << setw(width) << h << "\t"
                 << setw(width) << result_h1[0] << "\t"
                 << setw(width) << (result_h3[0] - result_h2[0])/(result_h2[0]-result_h1[0]) << "\t"
                 << setw(width) << result_h1[1] << "\t"
                 << setw(width) << (result_h3[1] - result_h2[1])/(result_h2[1]-result_h1[1]) << "\t"
                 << endl;

        result_h3 = result_h2;
        result_h2 = result_h1;

    }



    return 0;
}

