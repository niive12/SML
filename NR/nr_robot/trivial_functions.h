#ifndef TRIVIAL_FUNCTIONS_H
#define TRIVIAL_FUNCTIONS_H

#endif // TRIVIAL_FUNCTIONS_H

//functions are added in the class so that they can access the private data

template <class T>
double NRvector<T>::length()
{
	double sum = 0;

	for( int i = 0; i < nn; i++)
	{
		sum += pow(v[i],(double)2);
	}

	return sqrt(sum);
}

template <class T>
void NRvector<T>::print()
{
	for( int i = 0; i < nn; i++)
	{
		cout <<  v[i] << " ";

	}
	cout << endl;
}


template <class T>
void NRmatrix<T>::print()
{
	cout.precision(6);
	cout << setiosflags( ios::fixed );
	for( int i = 0; i < nn; i++)
	{
		for( int j = 0; j < mm; j++)
		{
			if ( v[i][j] >= 0 )
					cout << " ";
			cout << v[i][j] << "\t";
		}
		cout << endl;
	}
}
