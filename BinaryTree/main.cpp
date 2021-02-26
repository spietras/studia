#include <iostream>
#include <limits>
#include <string>
#include <cassert>
#include "binarytree.h"

using namespace std;

void initTest()
{
    BinaryTree a(5);
    assert(a.contains(5));
    assert(a.getHeight() == 1);
    assert(a.getHighest() == 5);
    assert(a.getLowest() == 5);
    assert(a.getNodeCount() == 1);
    assert(a.getRootCopy().getNumber() == 5);
    assert(a.getRootCopy().getLeftChild() == nullptr);
    assert(a.getRootCopy().getRightChild() == nullptr);

    BinaryTree b(1, {2,-5,10});
    assert(b.contains(1) && b.contains(2) && b.contains(-5) && b.contains(10));
    assert(b.getHeight() == 3);
    assert(b.getHighest() == 10);
    assert(b.getLowest() == -5);
    assert(b.getNodeCount() == 4);
    assert(b.getRootCopy().getNumber() == 1);
}

void copyTest()
{
    BinaryTree a(5, {2, 10, 0, 26});
    BinaryTree b = a;
    assert(b.contains(5) && b.contains(2) && b.contains(10) && b.contains(0) && b.contains(26));
    assert(b.getNodeCount() == 5);

    b.removeNode(2);
    b.removeNode(26);

    assert(!b.contains(2) && !b.contains(26));
    assert(a.contains(2) && a.contains(26));

    b.addNodes({-1, -5});
    assert(b.contains(-1) && b.contains(-5));
    assert(!a.contains(-1) && !a.contains(-5));
}

void addTest()
{
    BinaryTree t(5, {0, 1, 3});
    assert(!t.contains(2) && !t.contains(4));
    t.addNodes({2, 4});
    assert(t.contains(2) && t.contains(4));
}

void nodeTest()
{
    BinaryTree t(5, {0, 1, 3});
    assert(t.findNodeCopyPointer(10) == nullptr);
    assert(t.findNodeCopyPointer(5)->getNumber() == 5);
    assert(t.findNodeCopyPointer(1)->getNumber() == 1);

    assert(t.getRootCopy().getNumber() == 5);
}

void operatorPlusTest()
{
    BinaryTree a(5, {0,1,10});
    BinaryTree b(6, {0, 2, 9});

    BinaryTree c = a + b;
    assert(!a.contains(6) && !a.contains(2) && !a.contains(9));
    assert(!b.contains(5) && !b.contains(1) && !b.contains(10));

    assert(c.contains({5, 0, 1, 10, 6, 2, 9}));

    a += b;

    assert(a.contains({5,0,1,10,6,2,9}));
    assert(!b.contains(5) && !b.contains(1) && !b.contains(10));
}

void operatorMinusTest()
{
    BinaryTree a(5, {0,1,10});
    BinaryTree b(0, {1, 2, 4});

    BinaryTree c = a - b;

    assert(a.contains({5,0,1,10}));
    assert(c.contains({5, 10}) && !c.contains(0) && !c.contains(1));

    a -= b;

    assert(a.contains({5, 10}));
    assert(!a.contains(0) && !a.contains(1));
}

void compareTest()
{
    BinaryTree a(1, {2,3,4});
    BinaryTree b(5, {3,4,6,7});

    assert(a != b);
    assert(a < b);
    assert(b > a);
    assert(b >= a);
    assert(a <= b);
}

void operatorSubscriptTest()
{
    BinaryTree t(0, {1,2,3,4,5});
    assert(t[0] != nullptr);
    assert(t[0]->getNumber() == 0);
    assert(t[6] == nullptr);
    assert(t[3]->getNumber() == 3);
}

void removeTest()
{
    BinaryTree t(5, {1, 2, 7, 8});

    t.removeNodes({2, 7});
    assert(!t.contains(2) && !t.contains(7));

    t.removeNode(5);
    assert(t.contains(5) && t.getRootCopy().getNumber() == 5);
}

void changeTest()
{
    BinaryTree t(5, {0, 10, -3, 3, 7, 13});

    t.changeNumber(5, 10);
    assert(t.contains(5));

    t.changeNumber(5, 20);
    assert(t.contains(5) && !t.contains(20));

    t.changeNumber(5, 4);
    assert(!t.contains(5) && t.contains(4));

    t.changeNumber(13, 1000000);
    assert(!t.contains(13) && t.contains(1000000));
}

void test()
{
    initTest();
    copyTest();
    addTest();
    removeTest();
    nodeTest();
    changeTest();
    operatorPlusTest();
    operatorMinusTest();
    compareTest();
    operatorSubscriptTest();

    cout << "All test passed" << endl;
}

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
    test();

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
