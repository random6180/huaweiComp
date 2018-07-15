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
	treenode(int col, float value, vector<float> result) :
		col(col), value(value), left(nullptr), right(nullptr), result(result) { }


public:
	int col;		//��col��Ϊ��������
	float value;		//��С�ڵ���value��Ϊ�������ݣ�������Ϊ������������Ϊ������
	treenode* left;
	treenode* right;
	vector<float> result; //ֻ��Ҷ�ڵ������ֵ
};


pair<vector<vector<float> >, vector<vector<float> >> divideTwo(vector<vector<float> >& inputvector,int col, float value) {	//��col��value�������Ϊ������
	vector<vector<float> > leftvector;
	vector<vector<float> > rightvector;
	for (unsigned int i = 0; i < inputvector.size(); i++) {
		if (inputvector[i][col] <= value)
			leftvector.push_back(inputvector[i]);
		else
			rightvector.push_back(inputvector[i]);
	}
	pair<vector<vector<float> >, vector<vector<float> >> a(leftvector, rightvector);
	return a;
}

float squareSum(vector<vector<float> >& leftvector) {
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


vector<float> divideValue(vector<float> values) {		//��������
	if (values.size() <= 1)
		return values;
	vector<float> dividevalues;
	sort(values.begin(), values.end());
	float delta = (values[values.size() - 1] - values[0]) / (values.size() - 1);
	for (unsigned int i = 0; i < values.size(); i++) {
		dividevalues.push_back(values[0] + delta*i);
	}
	return dividevalues;
}


vector<float> getColValues(vector<vector<float> >& inputvector, unsigned int col) {
	vector<float> colvalues;
	for (unsigned int i = 0; i < inputvector.size(); i++) {
		colvalues.push_back(inputvector[i][col]);
	}
	return colvalues;
}




treenode* buildTree(vector<vector<float> >& inputvector) {							//���ܻᳬʱ
	treenode *node = new treenode();
	float minsquaresum = squareSum(inputvector);
	if (inputvector.size() <= 1 || minsquaresum == 0.0) {								//Ҷ���,ֻ��һ�����������������ͬ
		for (unsigned int i = 0; i < inputvector.size(); i++)
			(node->result).push_back(inputvector[i][inputvector[0].size() - 1]);
		return node;
	}

	pair<vector<vector<float> >, vector<vector<float> >> a;
	vector<vector<float> > leftvector;
	vector<vector<float> > rightvector;

	vector<vector<float> > finalleftvector;
	vector<vector<float> > finalrightvector;

	
	float squaresum = 0.0;

	unsigned int thecol = -1;
	float value = 0.0;
	bool flag = false;				//���������flagΪtrue

	vector<float> colvalues;
	vector<float> sortvalues;

	for (unsigned col = 0; col < inputvector[0].size() - 1; col++) {
		colvalues = getColValues(inputvector, col);		//��yֵ
		sortvalues = divideValue(colvalues);
		for (unsigned j = 0; j < inputvector.size(); j++) {
			a = divideTwo(inputvector, col, sortvalues[j]);		//�з�
			leftvector = a.first;
			rightvector = a.second;
			squaresum = squareSum(leftvector) + squareSum(rightvector);	//�����޷��ֿ�������
			if (squaresum < minsquaresum) {
				flag = true;
				minsquaresum = squaresum;
				thecol = col;
				value = sortvalues[j];
				finalleftvector = leftvector;
				finalrightvector = rightvector;
			}
		}
	}

	if (flag == false) {								//Ҷ���,ֻ��һ�����������������ͬ
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




vector<vector<float> > getInputValues(vector<vector<float> >& inputvector) {
	vector<vector<float> > inputs;
	vector<float> row;
	for (unsigned int i = 0; i < inputvector.size(); i++) {
		for (unsigned int j = 0; j < inputvector[0].size() - 1; j++)
			row.push_back(inputvector[i][j]);
		inputs.push_back(row);
		row.clear();
	}
	return inputs;
}



vector<vector<float> > divideserial(vector<float>& flavor, int num) {//��������Ϊnum+1ά�ģ��������һάΪ���
	vector<float> input(num+1, 0);
	vector<vector<float> > inputvector;
	for (unsigned int i = 0; i < flavor.size() - num; i++) {
		for (int j = 0; j < num + 1; j++) {
			input[j] = flavor[i + j];
		}
		inputvector.push_back(input);
	}
	return inputvector;
}


void drawTree(treenode& node, string space=" ") {
	if (node.col != -1) {			//��֧�ڵ�
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


float preOutput(treenode* node, vector<float>& input) {			//��������������������������
	int col = node->col;
	float value = node->value;
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


vector<float> continuesOutput(treenode* node, vector<float>& input, int days) {		//Ԥ�⼸�����
	vector<float> predays;
	float result;
	for (int i = 0; i < days; i++) {
		result = preOutput(node, input);
		predays.push_back(result);
		for (unsigned int j = 0; j < input.size()-1; j++)			//����������
			input[j] = input[j + 1];
		input[input.size() - 1] = result;
	}
	return predays;
}


vector<float> getInput(vector<float>& input, int num) {	//numΪ��������ά��
	vector<float> getinput;
	for (int i = 0; i < num; i++) {
		getinput.push_back(input[input.size() - num + i]);
	}
	return getinput;
}


float squareDiff(vector<float>& result) {		//����
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
	if (node->left->col != -1)			//��֧���
		prune(node->left, mingain);
	if (node->right->col != -1)			//��֧���
		prune(node->right, mingain);

	if ((node->left->col == -1) && (node->right->col == -1)) {
		vector<float> mergeresult;
		float leftsquarediff = squareDiff(node->left->result);
		float rightsquarediff = squareDiff(node->right->result);
		for (unsigned int i = 0; i < node->left->result.size(); i++) {
			mergeresult.push_back(node->left->result[i]);
		}
		for (unsigned int i = 0; i < node->right->result.size(); i++) {
			mergeresult.push_back(node->right->result[i]);
		}
		float mergesquarediff = squareDiff(mergeresult);
		if (mergesquarediff - leftsquarediff - rightsquarediff < mingain) {		//�����С����
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


float autocorr(vector<float>& serials, unsigned int m) {
	if (serials.size() - m <= 0)
		return -1;
	float sum = 0.0;
	for (unsigned int i = m; i < serials.size(); i++) {
		sum += serials[i] * serials[i - m];
	}
	return sum / (serials.size() - m);
}


float gbdtPreOutput(vector<treenode*> nodevector, vector<float>& input) {
	float sum = 0.0;
	for (unsigned int i = 0; i < nodevector.size(); i++) {
//		cout << "gbdt������" << i << ":" << preOutput(nodevector[i], input) << endl;
		sum += preOutput(nodevector[i], input);

	}
	return sum;
}


vector<treenode*> gbdt(vector<vector<float> >& inputvector, int iter, float treedif=1.5) {
	vector<treenode*> nodevector;
	treenode* node = new treenode;		//nodeΪ����
	vector<float> results;
	vector<float> colvalues;
	vector<float> sortvalues;
	float sum2 = 0.0;
	float min = 10000000;
	float f0;

	colvalues = getColValues(inputvector, inputvector[0].size() - 1);		//��yֵ
	sortvalues = divideValue(colvalues);					//���ȷָ�����	
	for (unsigned int i = 0; i < sortvalues.size(); i++) {
		for (unsigned int j = 0; j < colvalues.size(); j++)
			sum2 += (colvalues[j] - sortvalues[i])*(colvalues[j] - sortvalues[i]);
		if (sum2 < min) {
			min = sum2;
			f0 = sortvalues[i];
		}
		sum2 = 0;
	}

	node->result.push_back(f0);
	nodevector.push_back(node);

	vector<vector<float> > inputs;
	inputs = getInputValues(inputvector);
	float temp = 0.0;


	for (int k = 0; k < iter; k++) {
		cout << "��������:" << k << endl;
		for (unsigned int t = 0; t < inputvector.size(); t++) {
			temp = gbdtPreOutput(nodevector, inputs[t]);
	//		temp = preOutput(nodevector[k], inputs[t]);
			cout << temp << ' ';
			inputvector[t][inputvector[0].size() - 1] = colvalues[t] - temp;	//����в�
		}
		cout << endl;

		vector<float> tempcolvalues = getColValues(inputvector, inputvector[0].size() - 1);
		float sumdif = squareDiff(tempcolvalues);
		cout << "����:" << sumdif << endl;

		node = buildTree(inputvector);
		prune(node, 10/sumdif);
		nodevector.push_back(node);
	}

	return nodevector;

}









int main()
{
	int a[31] = { 3,  6,  2,  2,  0,  3,  4,  7, 26,  3,  4,  0,  0,  4,  1,  5,  3,
		20,  3,  0,  6,  2,  4,  7,  7,  2,  1, 12,  2,  3,  1
	};

	int b[7] = { 2,  5, 19,  1,  4,  0,  0 };

	vector<float> testflavor(a, a + 31);
	vector<vector<float> > inputvector;

	

	unsigned int num;			//ѡȡ����ά��
//	int result;
	vector<float> resultdays;

	float max = 0.0;
	float corr;
	for (unsigned int m = 1; m < 15; m++) {
		corr = autocorr(testflavor, m);
		if (corr > max) {
			max = corr;
			num = m;
		}
	}
	num = 7;
	cout << "num:" << num << endl;
//	vector<float> inputtest = getInput(testflavor, num);
	vector<float> inputtest(a, a + 7);
	inputvector = divideserial(testflavor, num);

	vector<treenode*> nodevector;

	treenode *node;

	/*cout << "�����ϵ��:" << endl;
	for (unsigned int m = 1; m < testflavor.size(); m++) {
		cout << m << " : " << autocorr(testflavor, m) << endl;
	}*/

	float treeresult;
	float gbdtresult;

	node = buildTree(inputvector);
//	prune(node);
	treeresult = preOutput(node, inputtest);
	cout << "������:" << treeresult << endl;

	nodevector = gbdt(inputvector,15);
	gbdtresult = gbdtPreOutput(nodevector, inputtest);
	cout << "GBDT:" << gbdtresult << endl;

	/*cout << "��֦��:" << endl;
	drawTree(*node);
	resultdays = continuesOutput(node, inputtest, 7);

	cout << "Ԥ����:" << endl;
	for (unsigned int i = 0; i < resultdays.size(); i++) {
		cout << resultdays[i] << ' ';
	}
	cout << endl;*/

	return 0;

}


//int main() {
//	float a[2][3] = { {0,0,1},{0,0,0} };
//	float b[3][2] = { {0,0},{1,0},{0,2} };
//
//	vector<vector<float> > aa;
//	aa.push_back(vector<float>(a[0], a[0] + 3));
//	aa.push_back(vector<float>(a[1], a[1] + 3));
//	vector<vector<float> > bb;
//	bb.push_back(vector<float>(b[0], b[0] + 2));
//	bb.push_back(vector<float>(b[1], b[1] + 2));
//	bb.push_back(vector<float>(b[2], b[2] + 2));
//
//	Matrix A(aa);
//	Matrix B(bb);
//	Matrix v;
//	v = A*B;
//	int pc = 3;
//
//	cout << v << endl;
//
//	pair<Matrix, Matrix> pr;
//	pr = factorize(v, 3);
//	cout << pr.first << endl << endl;
//	cout << pr.second << endl << endl;
//	cout << pr.first*pr.second << endl;
//
//	return 0;
//}