#include <iostream>
#include <limits>
#include <string>
#include "binarytree.h"

using namespace std;

void printTree(const BinaryTree& tree)
{
	cout << endl <<"Nodes: " << tree.getNodeCount() << ", Height: " << tree.getHeight() << ", Lowest: " << tree.getLowest() << ", Highest: " << tree.getHighest() << endl;
	cout << "Preorder: " << endl << tree << endl;
	cout << "Ascending: " << endl;
	tree.printAscending();
	cout << endl;
	cout << "Descending: " << endl;
	tree.printDescending();
	cout << endl << endl << "Structure:" << endl;
	tree.printPretty();
}

int getNumber(string message, int min = numeric_limits<int>::min(), int max = numeric_limits<int>::max())
{
	int n;
	cout << message << endl;
	while(true)
	{
		if(cin >> n && (n >= min && n <= max)) break;

		cout << "Invalid input. Write again" << endl;
		cin.clear();
		cin.ignore(numeric_limits<streamsize>::max(), '\n');
	}
	cin.clear();
	cin.ignore(numeric_limits<streamsize>::max(), '\n');

	return n;
}

BinaryTree initializeTree()
{
	int n = getNumber("Write root number: ");

	BinaryTree tree(n);

	cout << "Write numbers separated with space to add them to the tree: " << endl;
	cin >> tree;
	cin.clear();
	cin.ignore(numeric_limits<streamsize>::max(), '\n');

	return tree;
}

void add(BinaryTree& tree)
{
	cout << "Write numbers separated with space to add them to the tree: " << endl;
	cin >> tree;
	cin.clear();
	cin.ignore(numeric_limits<streamsize>::max(), '\n');
}

void remove(BinaryTree& tree)
{
	int n;
	vector<int> values;
	cout << "Write numbers separated with space to remove them from the tree: " << endl;
	while(cin.peek() != '\n')
	{
		if(cin.peek() == ' ' || cin.peek() == '\t')
		{
			cin.get();
			continue;
		}

		if(cin >> n)
		{
			values.push_back(n);
			continue;
		}

		break;
	}

	cin.clear();
	cin.ignore(numeric_limits<streamsize>::max(), '\n');

	tree.removeNodes(values);
}

void change(BinaryTree& tree)
{
	int o = getNumber("Write value to change: ");
	int n = getNumber("Write value to change to: ");

	tree.changeNumber(o, n);
}

int switchTrees(const vector<BinaryTree>& trees)
{
	cout << "Trees indexes: ";
	for(unsigned int i = 0; i < trees.size(); i++) cout << " " << i << " ";
	cout << endl;

	int n = getNumber("Write chosen tree index: ", 0, trees.size());

	return n;
}

void addTrees(vector<BinaryTree>& trees, int current)
{
	cout << "Trees indexes: ";
	for(unsigned int i = 0; i < trees.size(); i++) cout << " " << i << " ";
	cout << endl;

	int n = getNumber("Write index of tree you want to add to current tree: ", 0, trees.size());

	trees.at(current) += trees.at(n);
}

void subtractTrees(vector<BinaryTree>& trees, int current)
{
	cout << "Trees indexes: ";
	for(unsigned int i = 0; i < trees.size(); i++) cout << " " << i << " ";
	cout << endl;

	int n = getNumber("Write index of tree you want to subtract from current tree: ", 0, trees.size());

	trees.at(current) -= trees.at(n);
}

int main()
{
	vector<BinaryTree> trees;
	trees.push_back(initializeTree());
	int current = 0;
	
	bool exit = false;
	
	while(!exit)
	{
		cout << "Choose what to do: " << endl;
		cout << "1. Initialize new tree" << endl;
		cout << "2. Add nodes" << endl;
		cout << "3. Remove nodes" << endl;
		cout << "4. Change node" << endl;
		cout << "5. View current tree" << endl;
		cout << "6. Switch trees" << endl;
		cout << "7. Add another tree to current tree" << endl;
		cout << "8. Subtract another tree from current tree" << endl;

		int n = cin.get() - '0';
		cin.clear();
		cin.ignore(numeric_limits<streamsize>::max(), '\n');

		switch(n)
		{
		case 1:
			trees.push_back(initializeTree());
			current = trees.size() - 1;
			break;
		case 2:
			add(trees.at(current));
			break;
		case 3:
			remove(trees.at(current));
			break;
		case 4:
			change(trees.at(current));
			break;
		case 5:
			printTree(trees.at(current));
			break;
		case 6:
			current = switchTrees(trees);
			break;
		case 7:
			addTrees(trees, current);
			break;
		case 8:
			subtractTrees(trees, current);
			break;
		default:
			exit = true;
		}
	}
}
