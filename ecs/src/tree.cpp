#include <iostream>
#include <stdlib.h>
#include <vector>
#include <string>
#include <algorithm>
#include <ctime>
using namespace std;



class Matrix {
	friend	ostream& operator << (ostream& os, Matrix& matrix);
public:
	Matrix() {}
	Matrix(vector<vector<float>> &matrix) : matrix(matrix) {}
	Matrix(int row, int col) {
		vector<vector<float> > temp(row, vector<float>(col, 0.0));
		matrix = temp;
		srand((unsigned)time(NULL));
		for (int i = 0; i < row; i++)
			for (int j = 0; j < col; j++)
				matrix[i][j] = (float)((rand() % 100) / 100.0f);
	}


	int rowsize() {
		return matrix.size();
	}
	int colsize() {
		return matrix[0].size();
	}

	void initMatrix(vector<vector<float> >& rhs) {
		matrix.clear();
		matrix = rhs;
	}


	Matrix transpose() {
		int newrow = colsize();
		int newcol = rowsize();
		Matrix temp;
	//	vector<float> temp(newcol, 0.0);
		vector<vector<float> > trans(newrow, vector<float>(newcol, 0));
		for (int i = 0; i < rowsize(); i++) {
			for (int j = 0; j < colsize(); j++)
				trans[j][i] = matrix[i][j];
		}
		temp.initMatrix(trans);
		return temp;
	}

	Matrix operator+(Matrix& rhs) {
		vector<vector<float> > trans(rowsize(), vector<float>(colsize(), 0));
		Matrix temp;
		if (rowsize() != rhs.rowsize() || colsize() != rhs.colsize()) {
			cout << "row or col error!" << endl;
			return *this;
		}
		for (int i = 0; i < rhs.rowsize(); i++) {
			for (int j = 0; j < rhs.colsize(); j++) {
				trans[i][j] += rhs.matrix[i][j];
			}
		}
		temp.initMatrix(trans);
		return temp;
	}

	Matrix operator*(Matrix& rhs) {
		if (colsize() != rhs.rowsize()) {
			cout << "row is not equal to col,error!" << endl;
			return *this;
		}
		vector<vector<float> > finalresult;
		vector<float> result;
		Matrix tempmatrix;
		float temp = 0;
		for (int i = 0; i < rowsize(); i++) {
			for (int j = 0; j < rhs.colsize(); j++) {
				for (int k = 0; k < colsize(); k++) 
					temp += (matrix[i][k] * rhs.matrix[k][j]);
				result.push_back(temp);
				temp = 0.0;
			}
			finalresult.push_back(result);
			result.clear();
		}
		tempmatrix.initMatrix(finalresult);
		return tempmatrix;
	}

	Matrix& operator=(Matrix& rhs) {
		initMatrix(rhs.matrix);
		return *this;
	}


public:
	vector<vector<float> > matrix;
};


ostream& operator << (ostream& os, Matrix& matrix) {
	for (int i = 0; i < matrix.rowsize(); i++) {
		for (int j = 0; j < matrix.colsize(); j++) {
			os << matrix.matrix[i][j] << ' ';
		}
		os << endl;
	}
	return os;
}

float difcost(Matrix& A, Matrix& B) {
	float dif = 0;
	for (int i = 0; i < A.rowsize(); i++) {
		for (int j = 0; j < A.colsize(); j++)
			dif += ((A.matrix[i][j] - B.matrix[i][j])*(A.matrix[i][j] - B.matrix[i][j]));
	}
	return dif;
}



class Array {
public:
	Array() {}
	Array(Matrix& rhs) : matrix(rhs) {}


	Array operator*(Array& rhs) {
		vector<vector<float> > trans(rhs.matrix.rowsize(), vector<float>(rhs.matrix.colsize(), 0));
		Matrix temp(trans);
		Array arrays(temp);
		for (int i = 0; i < rhs.matrix.rowsize(); i++)
			for (int j = 0; j < rhs.matrix.colsize(); j++)
				arrays.matrix.matrix[i][j] = matrix.matrix[i][j] * rhs.matrix.matrix[i][j];
		return arrays;
	}

	Array operator/(Array& rhs) {
		vector<vector<float> > trans(rhs.matrix.rowsize(), vector<float>(rhs.matrix.colsize(), 0));
		Matrix temp(trans);
		Array arrays(temp);
		for (int i = 0; i < rhs.matrix.rowsize(); i++)
			for (int j = 0; j < rhs.matrix.colsize(); j++)
				arrays.matrix.matrix[i][j] = matrix.matrix[i][j] / rhs.matrix.matrix[i][j];
		return arrays;
	}


public:
	Matrix matrix;

};


pair<Matrix, Matrix> factorize(Matrix& v, int pc = 10, int iter = 100) {
	pair<Matrix, Matrix> pr;
	int ic = v.rowsize();
	int fc = v.colsize();

	Matrix w(ic, pc);
	Matrix h(pc, fc);
	Matrix wh, hn, hd, wn, wd;
	float cost = 0.0;

	for (int i = 0; i < iter; i++) {
		wh = w * h;
		cost = difcost(v, wh);
		if (i % 10 == 0) {
			cout << cost << endl;
	//		cout << wh << endl;
		}
		if (cost < 0.00001)
			break;
		hn = w.transpose()*v;
		hd = w.transpose()*w*h;
		h = (Array(h)*Array(hn) / Array(hd)).matrix;
		wn = v*h.transpose();
		wd = w*h*h.transpose();
		w = (Array(w)*Array(wn) / Array(wd)).matrix;
	}
	pr.first = w;
	pr.second = h;
	return pr;
}






class treenode {
public:
	treenode() : col(-1), value(0), left(nullptr), right(nullptr) {}
	treenode(int col, int value, vector<int> result) :
		col(col), value(value), left(nullptr), right(nullptr), result(result) { }


public:
	int col;		//以col作为特征划分
	int value;		//以小于等于value作为划分依据，满足则为左子树，否则为右子树
	treenode* left;
	treenode* right;
	vector<int> result; //只对叶节点存放输出值
};


pair<vector<vector<int> >, vector<vector<int> >> divideTwo(vector<vector<int> >& inputvector,int col, int value) {	//以col和value将输入分为两部分
	vector<vector<int> > leftvector;
	vector<vector<int> > rightvector;
	for (unsigned int i = 0; i < inputvector.size(); i++) {
		if (inputvector[i][col] <= value)
			leftvector.push_back(inputvector[i]);
		else
			rightvector.push_back(inputvector[i]);
	}
	pair<vector<vector<int> >, vector<vector<int> >> a(leftvector, rightvector);
	return a;
}

float squareSum(vector<vector<int> >& leftvector) {
	if (leftvector.size() == 0)
		return 0;
	float sum = 0;
	float sum2 = 0;
	for (unsigned int i = 0; i < leftvector.size(); i++) {
		sum += leftvector[i][leftvector[0].size() - 1];
		sum2 += (leftvector[i][leftvector[0].size() - 1] * leftvector[i][leftvector[0].size() - 1]);
	}
//	cout << (sum2 - sum*sum / leftvector.size()) << endl;
	return (sum2 - sum*sum/ leftvector.size());
}




treenode* buildTree(vector<vector<int> >& inputvector) {							//可能会超时
	treenode *node = new treenode();
	float minsquaresum = squareSum(inputvector);
	if (inputvector.size() <= 1 || minsquaresum == 0.0) {								//叶结点,只有一个向量，或者输出相同
		for (unsigned int i = 0; i < inputvector.size(); i++)
			(node->result).push_back(inputvector[i][inputvector[0].size() - 1]);
		return node;
	}

	pair<vector<vector<int> >, vector<vector<int> >> a;
	vector<vector<int> > leftvector;
	vector<vector<int> > rightvector;

	vector<vector<int> > finalleftvector;
	vector<vector<int> > finalrightvector;

	
	float squaresum = 0.0;

	unsigned int thecol = -1;
	int value = 0;
	bool flag = false;				//正常情况下flag为true

	for (unsigned col = 0; col < inputvector[0].size() - 1; col++) {
		for (unsigned j = 0; j < inputvector.size(); j++) {
			a = divideTwo(inputvector, col, inputvector[j][col]);		//切分
			leftvector = a.first;
			rightvector = a.second;
			squaresum = squareSum(leftvector) + squareSum(rightvector);	//存在无法分开的情形
			if (squaresum < minsquaresum) {
				flag = true;
				minsquaresum = squaresum;
				thecol = col;
				value = inputvector[j][col];
				finalleftvector = leftvector;
				finalrightvector = rightvector;
			}
		}
	}

	if (flag == false) {								//叶结点,只有一个向量，或者输出相同
		for (unsigned int i = 0; i < inputvector.size(); i++)
			(node->result).push_back(inputvector[i][inputvector[0].size() - 1]);
		return node;
	}


	node->col = thecol;
	node->value = value;
//	treenode left(buildTree(finalleftvector));
// 	treenode right(buildTree(finalrightvector));
	node->left = buildTree(finalleftvector);
	node->right = buildTree(finalrightvector);

	return node;
}



vector<vector<int> > divideserial(vector<int>& flavor, int num) {//输入向量为num+1维的，其中最后一维为输出
	vector<int> input(num+1, 0);
	vector<vector<int> > inputvector;
	for (unsigned int i = 0; i < flavor.size() - num; i++) {
		for (int j = 0; j < num + 1; j++) {
			input[j] = flavor[i + j];
		}
		inputvector.push_back(input);
	}
	return inputvector;
}


void drawTree(treenode& node, string space=" ") {
	if (node.col != -1) {			//分支节点
		cout << "(" << node.col << "," << node.value << ")" << endl;
		cout << space;
		cout << "left:";
		drawTree(*node.left, space+' ');
		cout << space;
		cout << "right:";
		drawTree(*node.right, space+' ');

	}
	else {
		cout << "leaf: ";
		for (unsigned int i = 0; i < node.result.size(); i++) {
			cout << node.result[i] << ' ';
		}
		cout << endl;
	}
}


int preOutput(treenode* node, vector<int>& input) {			//给定决策树，输入变量，求输出
	int col = node->col;
	int value = node->value;
	if (col == -1) {
		float sum = 0.0;
		if (node->result.size() == 0) {
			cout << "node wrong!";
			return -1;
		}
		sort(node->result.begin(), node->result.end());
		return node->result[(node->result.size()) / 2];
	//	for (unsigned int i = 0; i < node->result.size(); i++) {
	//		sum += (node->result[i]);
	//	}
	//	return static_cast<int>(sum / (node->result.size()) + 0.5);
	}
	if (input[col] <= value)
		return preOutput(node->left, input);
	else
		return preOutput(node->right, input);
}


vector<int> continuesOutput(treenode* node, vector<int>& input, int days) {		//预测几天输出
	vector<int> predays;
	int result;
	for (int i = 0; i < days; i++) {
		result = preOutput(node, input);
		predays.push_back(result);
		for (unsigned int j = 0; j < input.size()-1; j++)			//对输入左移
			input[j] = input[j + 1];
		input[input.size() - 1] = result;
	}
	return predays;
}


vector<int> getInput(vector<int>& input, int num) {	//num为输入向量维数
	vector<int> getinput;
	for (int i = 0; i < num; i++) {
		getinput.push_back(input[input.size() - num + i]);
	}
	return getinput;
}


float squareDiff(vector<int>& result) {		//方差
	if (result.size() == 0)
		return 0;
	float sum = 0;
	float sum2 = 0;
	for (unsigned int i = 0; i < result.size(); i++) {
		sum += result[i];
		sum2 += (result[i] * result[i]);
	}
	//	cout << (sum2 - sum*sum / leftvector.size()) << endl;
	return (sum2 - sum*sum / result.size())/result.size();
}


void prune(treenode* node, float mingain = 0.9) {
	if (node->col == -1)
		return;
	if (node->left->col != -1)			//分支结点
		prune(node->left, mingain);
	if (node->right->col != -1)			//分支结点
		prune(node->right, mingain);

	if ((node->left->col == -1) && (node->right->col == -1)) {
		vector<int> mergeresult;
		float leftsquarediff = squareDiff(node->left->result);
		float rightsquarediff = squareDiff(node->right->result);
		for (unsigned int i = 0; i < node->left->result.size(); i++) {
			mergeresult.push_back(node->left->result[i]);
		}
		for (unsigned int i = 0; i < node->right->result.size(); i++) {
			mergeresult.push_back(node->right->result[i]);
		}
		float mergesquarediff = squareDiff(mergeresult);
		if (mergesquarediff - leftsquarediff - rightsquarediff < mingain) {		//方差减小显著
			delete node->left;
			delete node->right;
			node->left = nullptr;
			node->right = nullptr;
			node->col = -1;
			node->value = 0;
			node->result = mergeresult;
		}
	}
}


float autocorr(vector<int>& serials, unsigned int m) {
	if (serials.size() - m <= 0)
		return -1;
	float sum = 0.0;
	for (unsigned int i = m; i < serials.size(); i++) {
		sum += serials[i] * serials[i - m];
	}
	return sum / (serials.size() - m);
}






//int main()
//{
//	int a[31] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
//		1, 0, 0, 0, 0, 0, 0, 0
//
//	};
//
//	int b[7] = { 2,  5, 19,  1,  4,  0,  0 };
//
//	vector<int> testflavor(a, a + 30);
//	vector<vector<int> > inputvector;
//
//	
//
//	unsigned int num;			//选取特征维数
////	int result;
//	vector<int> resultdays;
//
//	float max = 0.0;
//	float corr;
//	for (unsigned int m = 1; m < 15; m++) {
//		corr = autocorr(testflavor, m);
//		if (corr > max) {
//			max = corr;
//			num = m;
//		}
//	}
//	vector<int> inputtest = getInput(testflavor, num);
//	inputvector = divideserial(testflavor, num);
//
//	treenode *node;
//
//	cout << "自相关系数:" << endl;
//	for (unsigned int m = 1; m < testflavor.size(); m++) {
//		cout << m << " : " << autocorr(testflavor, m) << endl;
//	}
//
//	node = buildTree(inputvector);
//	cout << "剪枝前:" << endl;
//	drawTree(*node);
//	resultdays = continuesOutput(node, inputtest, 7);
//
//	cout << "预测结果:" << endl;
//	for (unsigned int i = 0; i < resultdays.size(); i++) {
//		cout << resultdays[i] << ' ';
//	}
//	cout << endl;
//
//	prune(node);
//	cout << "剪枝后:" << endl;
//	drawTree(*node);
//	resultdays = continuesOutput(node, inputtest, 7);
//
//	cout << "预测结果:" << endl;
//	for (unsigned int i = 0; i < resultdays.size(); i++) {
//		cout << resultdays[i] << ' ';
//	}
//	cout << endl;
//
//	return 0;
//
//}


int main() {
	float a[2][3] = { {0,0,1},{0,0,0} };
	float b[3][2] = { {0,0},{1,0},{0,2} };

	vector<vector<float> > aa;
	aa.push_back(vector<float>(a[0], a[0] + 3));
	aa.push_back(vector<float>(a[1], a[1] + 3));
	vector<vector<float> > bb;
	bb.push_back(vector<float>(b[0], b[0] + 2));
	bb.push_back(vector<float>(b[1], b[1] + 2));
	bb.push_back(vector<float>(b[2], b[2] + 2));

	Matrix A(aa);
	Matrix B(bb);
	Matrix v;
	v = A*B;
	int pc = 3;

	cout << v << endl;

	pair<Matrix, Matrix> pr;
	pr = factorize(v, 3);
	cout << pr.first << endl << endl;
	cout << pr.second << endl << endl;
	cout << pr.first*pr.second << endl;

	/*int ic = v.rowsize();
	int fc = v.colsize();

	Matrix w(ic, pc);
	Matrix h(pc, fc);
	Matrix wh, hn, hd, wn, wd;
	float cost = 0.0;

	Matrix temp;

	wh = w*h;
	cout << wh << endl;

	cost = difcost(v, wh);
	cout << cost << endl;
	
	hn = w.transpose()*v;
	cout << hn << endl;
	temp = w.transpose()*w;
	hd = temp*h;*/
//	hd = ((w.transpose()*w)*h);
//	h = (Array(h)*Array(hn) / Array(hd)).matrix;
//	wn = v*h.transpose();
//	wd = w*h*h.transpose();
//	w = (Array(w)*Array(wn) / Array(wd)).matrix;


//	cout << A << endl;
//	cout << B << endl;
//	cout << A+B << end;
//	cout << A*B << endl;
//	Matrix A()
	return 0;
}