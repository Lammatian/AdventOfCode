#include <iostream>

#include "util.h"

using namespace std;

ll file_checksum(ll file_id, ll start_pos, ll file_len) {
    return (2 * start_pos + file_len - 1) * file_len / 2 * file_id;
}

ll checksum(vector<int> dmap) {
    ll result = 0;
    size_t idx = 0;
    ll pos = 0;
    ll file_id = 0;
    bool freeblock = false;
    while (idx < dmap.size()) {
        if (!freeblock) {
            result += file_checksum(file_id, pos, dmap[idx]);
            pos += dmap[idx];
            file_id++;
        } else {
            ll freeblock_size = dmap[idx];
            while (freeblock_size > 0) {
                if (freeblock_size >= dmap.back()) {
                    // Partial fill of a free block
                    result += file_checksum((dmap.size() - 1) / 2, pos, dmap.back());
                    pos += dmap.back();
                    freeblock_size -= dmap.back();
                    dmap.pop_back();
                    dmap.pop_back();
                } else {
                    // Full fill of a free block
                    result += file_checksum((dmap.size() - 1) / 2, pos, freeblock_size);
                    dmap.back() -= freeblock_size;
                    pos += freeblock_size;
                    freeblock_size = 0;
                }
            }
        }

        freeblock = !freeblock;
        idx++;
    }

    return result;
}

ll checksum2(vector<int> dmap) {
    ll result = 0;
    vector<int> freespace_filled(dmap.size());
    for (size_t idx = dmap.size() - 1; idx > 0; idx -= 2) {
        ll file_id = idx / 2; 
        bool moved = false;
        ll start_pos = 0;
        for (size_t free_idx = 1; free_idx < idx; free_idx += 2) {
            start_pos += dmap[free_idx - 1]; 
            if (dmap[free_idx] >= dmap[idx]) {
                start_pos += freespace_filled[free_idx];
                result += file_checksum(file_id, start_pos, dmap[idx]);
                freespace_filled[free_idx] += dmap[idx];
                dmap[free_idx] -= dmap[idx];
                moved = true;
                break;
            }
            start_pos += dmap[free_idx] + freespace_filled[free_idx];
        }
        if (!moved) {
            result += file_checksum(file_id, start_pos, dmap[idx]);
        }
    }
    
    return result;
}

int main() {
    string dmap_str;
    cin >> dmap_str;
    vector<int> dmap;
    for (auto c: dmap_str) {
        dmap.push_back(c - '0');
    }
    cout << checksum(dmap) << "\n";
    cout << checksum2(dmap) << "\n";

    return 0;
}
