#include <iostream>
#include <vector>
#include <limits>
#include <algorithm>
#include <numeric>

class SonarSweep {
    std::vector<int> measurements;

  public:
    SonarSweep(std::istream& is) {
        for (int measurement; is >> measurement;) {
            measurements.push_back(measurement);
        }
    }

    int count_depth_increases(int sliding_window = 1) {
        int depth_increases = 0;
        for (size_t i = sliding_window; i < measurements.size(); ++i) {
            if (measurements[i - sliding_window] < measurements[i]) {
                depth_increases++; 
            }
        }

        return depth_increases;
    }
};

int main() {
    SonarSweep sonar_sweep(std::cin);
    std::cout << sonar_sweep.count_depth_increases() << std::endl;
    std::cout << sonar_sweep.count_depth_increases(3) << std::endl;

    return 0;
}
