#include <vector>
#include <queue>
#include <iostream>
#include <algorithm>

using namespace std;

class UnionFind {
    vector<int> parent;
    vector<int> rank;

public:
    UnionFind(int n){
        parent = vector<int>(n, -1); // parent[i] = -1 -> i is root.
        rank = vector<int>(n, 0);
    }

    int find(int n){
        vector<int> path;
        while(parent[n]!=-1){
            path.push_back(n);
            n = parent[n];
        }
        for(vector<int>::iterator it = path.begin(); it != path.end(); it++){
            parent[*it] = n;
            rank[*it] = 0;
        }
        return n;
    }

    void unify(int x, int y){
        int rx = find(x), ry = find(y);
        if(rx == ry) return;

        if(rank[rx] < rank[ry]){
            parent[rx] = ry;
        } else {
            parent[ry] = rx;
            if(rank[rx] == rank[ry]) rank[rx]++;
        }
    }

    bool same(int x, int y){
        return find(x) == find(y);
    }
};
