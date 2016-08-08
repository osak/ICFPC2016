#include <cstdio>

using namespace std;

int main() {
    char c;
    int sum = 0;
    
    while ((c = getchar()) != EOF) {
        if ((c >= '0' && c <= '9') || c == ',' || c == '/' || c == '-') sum++;
    }
    
    printf("%d\n", sum);
    
    return 0;
}
