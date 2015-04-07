#include <iostream>
#include <cmath>
#include <vector>

using namespace std;

vector<double> y_marks(vector<double> prev){
    vector<double> res(2);

    res[0] = prev[0]*prev[1];
    res[1] = -pow(prev[0],2);

    return res;
}


template <class T>
vector<double> euler_method(vector<T> &func, vector<double> init, double N, double h){

    vector<double> x_vals_prev = init;
    vector<double> x_vals_new = init;

    for(int n = 0; n < N; n++){
        for(int i = 0; i < func.size(); i++){
            x_vals_new = x_vals_prev + h*(func[i](x_vals_prev));
        }
        x_vals_prev = x_vals_new;
    }

    return x_vals_new;
}


int main()
{
    // make eulers!
    cout << "Hello World!" << endl;

    vector<double> initials(2);
    initials[0] = 1;
    initials[1] = 1;

    vector<double> result(2);

    result = euler_method(y_marks, initials, 20, 0.1);

    for(int i = 0; i < result.size();  i++){
        cout << result[i] << endl;
    }

    return 0;
}

