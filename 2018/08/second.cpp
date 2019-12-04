#include <iostream>
#include <fstream>
#include <vector>
#include <stack>

struct Node {
    Node(Node* p, int n, int m) : parent{p}, nodes{n}, meta{m} {};
    Node* parent;
    std::vector<Node*> children;
    std::vector<int> metaID;
    int nodes;
    int meta;
};

struct Count {
    int nodes;
    int meta;
};

int getValue(Node* node) {
    int result = 0;

    if (node->children.empty()) {
        for (auto &x: node->metaID) {
            result += x;
        }

        return result;
    }

    for (auto &x: node->metaID) {
        if (x < node->children.size() + 1) {
            result += getValue(node->children[x - 1]);
        }
    }

    return result;
}

int main() {
    std::fstream f("input.txt");
    std::stack<Count> s;

    int a, b;
    f >> a >> b;
    Node* root = new Node(nullptr, a, b);
    Node* curr = root;

    while (curr != nullptr) {
        if (curr->nodes == 0 && curr->meta == 0) {
            curr = curr->parent;
            continue;
        } else if (curr->nodes == 0) {
            while (curr->meta > 0) {
                int m;
                f >> m;
                curr->metaID.push_back(m);
                curr->meta--;
            }
        } else {
            f >> a >> b;
            Node* next = new Node(curr, a, b);
            curr->children.push_back(next);
            curr->nodes--;
            curr = next;
        }
    }

    std::cout << getValue(root) << std::endl;

    return 0;
}
