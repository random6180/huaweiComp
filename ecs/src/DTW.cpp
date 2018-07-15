#include <iostream>
#include <vector>
#include <string>
#include <cmath>
#include <algorithm>
using namespace std;


void limitToOne(vector<double>& limit) {
	vector<double> temp = limit;
	sort(temp.begin(), temp.end());
	double min = temp[0];
	double max = temp[temp.size() - 1];
	if (max == min) {
		for (unsigned int i = 0; i < limit.size() - 1; i++) {
			limit[i] = 1;
		}
	}
	else {
		for (unsigned int i = 0; i < limit.size() - 1; i++) {
			limit[i] = 1 / (max - min)*limit[i] - min / (max - min);
		}
	}
}


double distanceDTW(vector<double>& serialx, vector<double>& serialy) {
	vector<vector<double> > serialmatrix;
	vector<double> rowserial;
	for (unsigned int i = 0; i < serialx.size(); i++) {
		for (unsigned int j = 0; j < serialy.size(); j++) {
			rowserial.push_back(abs(serialx[i] - serialy[j]));
		}
		serialmatrix.push_back(rowserial);
		rowserial.clear();
	}

	for (int i = (int)serialx.size() - 1; i >= 0; i--) {
		for (int j = (int)serialy.size() - 1; j >= 0; j--) {
			if (i == serialmatrix.size() - 1 && j == serialmatrix[0].size() - 1)
				continue;
			else if (i == serialmatrix.size() - 1) {
				serialmatrix[i][j] += serialmatrix[i][j + 1];
			}
			else if (j == serialmatrix[0].size() - 1) {
				serialmatrix[i][j] += serialmatrix[i + 1][j];
			}
			else {
				serialmatrix[i][j] += min(min(serialmatrix[i][j + 1], serialmatrix[i + 1][j]), serialmatrix[i + 1][j + 1]);
			}
		}
	}
	
	return serialmatrix[0][0];
}


vector<int> findSim(vector<double>& flavorlist, int xnum) {
	vector<double> serialy;
	vector<double> serialx;
	vector<double> serialtest;
	vector<int> indextest;
	vector<int> index;
	double distmin = 1000;

	for (int i = 0; i < xnum; i++) {
		serialx.push_back(flavorlist[flavorlist.size() - xnum + i]);
	}
	vector<double> serialxtemp = serialx;
	limitToOne(serialxtemp);
	for (unsigned int u = 8; u > 4; u--) {		//8,7,6,5
		for (unsigned int i = 0; i < flavorlist.size() - u - xnum; i++) {
			for (unsigned int j = i; j < i + u; j++) {
				serialtest.push_back(flavorlist[j]);
				indextest.push_back(j);
			}
			vector<double> serialtesttemp = serialtest;
			limitToOne(serialtesttemp);
			double dist = distanceDTW(serialxtemp, serialtesttemp);
			cout << dist << endl;
			if (dist < distmin) {
				distmin = dist;
				serialy = serialtest;
				index = indextest;
			}
			serialtest.clear();
			indextest.clear();
		}
	}

	return index;
}



int main()
{
	vector<double> flavorlist = { 12,  6,  22,  2,  0,  3,  4,  7, 26,  3,  4,  0,  0,  4,  1,  5,  3,
		20,  3,  0,  6,  2,  4,  7,  7,  2,  1, 12,  2,  3,  1};		//以x为行，y为列
	vector<double> serialx = {4, 7, 7,  2,  1, 12,  2};
	vector<int> index;
	
	index = findSim(flavorlist, 7);
	for (unsigned int i = 0; i < index.size(); i++) {
		cout << flavorlist[index[i]] << ' ';
	}
	cout << endl;
	
	return 0;
}