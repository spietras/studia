#include <iostream>
#include <string>
#include <cassert>
#include "binarytree.h"

using namespace std;

void test()
{
	BinaryTree b(1);
	assert(b.getRootCopy().getNumber() == 1);
	assert(b.contains(1));
	assert(b.getHeight() == 1);
	assert(b.getHighest() == b.getLowest() == 1);
	assert(b.getNodeCount() == 1);
	vector<int> v = { 1 };
	assert(b.getValuesPreOrder() == v);
	assert(b[1]); //Check if b[1] exists
	assert(b[1]->getNumber() == 1);
	
	BinaryTree c(2, { 0,5,10 });
	assert(c.getRootCopy().getNumber() == 2);
	assert(c.getHighest() == 10);
	assert(c.getLowest() == 0);
	assert(c.getNodeCount() == 4);
	v = { 2,0,5,10 };
	assert(c.getValuesPreOrder() == v);
	assert(c.contains({ 0,2,5,10 }));
	assert(c.getHeight() == 3);
	assert(c[0] && c[2] && c[5] && c[10]);
	assert(c[0]->getNumber() == 0 && c[2]->getNumber() == 2 && c[5]->getNumber() == 5 && c[10]->getNumber() == 10);
	assert(c > b);
	assert(b < c);
	assert(b != c);
	assert(c != b);

	BinaryTree d = c;
	assert(d.getRootCopy().getNumber() == 2);
	assert(d.getHighest() == 10);
	assert(d.getLowest() == 0);
	assert(d.getNodeCount() == 4);
	assert(d.getValuesPreOrder() == v);
	assert(d.contains(c));
	assert(d.getHeight() == 3);
	assert(d[0] && d[2] && d[5] && d[10]);
	assert(d[0]->getNumber() == 0 && d[2]->getNumber() == 2 && d[5]->getNumber() == 5 && d[10]->getNumber() == 10);
	assert(d == c);
	assert(d != b);

	c.removeNode(5);
	assert(!c.contains(5) && d.contains(5)); //Check if editing c doesn't change d
	c.addNode(7);
	assert(c.contains(7) && !d.contains(7));
	assert(c != d);

	BinaryTree e(d.getRootCopy());
	assert(e.getRootCopy().getNumber() == 2);
	assert(e.getHighest() == 10);
	assert(e.getLowest() == 0);
	assert(e.getNodeCount() == 4);
	assert(e.getValuesPreOrder() == v);
	assert(e.contains(d));
	assert(e.getHeight() == 3);
	assert(e[0] && e[2] && e[5] && e[10]);
	assert(e[0]->getNumber() == 0 && e[2]->getNumber() == 2 && e[5]->getNumber() == 5 && e[10]->getNumber() == 10);
	assert(e == d);

	d.removeNode(5);
	assert(!d.contains(5) && e.contains(5)); //Check if editing d doesn't change e
	d.addNode(7);
	assert(d.contains(7) && !e.contains(7));
	assert(d != e);

	BinaryTree f(-10, { -5,-2 });
	f += e;
	assert(f.contains(e));
	assert(f > e);
	assert(f != e);

	f -= e;
	assert(!f.contains(e));

	f.changeNumber(-2, -1);
	assert(f.contains(-1) && !f.contains(-2));
	f.changeNumber(-5, -6);
	assert(f.contains(-6) && !f.contains(-5));

	cout << "Write numbers to add: " << endl;
	cin >> f;
	cin.clear();
	cin.ignore(numeric_limits<streamsize>::max(), '\n');
	cout << f << endl;

	cout << "All test passed" << endl;
}


int main()
{

#ifdef _DEBUG
	test();
#endif

	cin.ignore();
}
