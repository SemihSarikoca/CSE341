int weighted_sum(int a, int b);

int main() {
    int x = 10;
    int y = 20;
    int sum;

    sum = weighted_sum(x, y);

    printf("The sum is: %d\n", sum);
    return 0;
}

int weighted_sum(int a, int b) {
    int result = 0;
    int i = 0;
    for (i = 0; i < a; i++) {
	if (i % 2 == 0) {
	   result += i;    
	}
    }
    i = 0;
    while (i < b) {
	if (i % 2 != 0) {
	   result += i;    
	}
	i++;
    }
    return result;
}