#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

void basic_multiply(int *a, int *b, int *c, int len_a, int len_b, int *len_c) {
    memset(c, 0, (len_a + len_b) * sizeof(int));
    
    for (int i = 0; i < len_a; i++) {
        for (int j = 0; j < len_b; j++) {
            c[i + j] += a[i] * b[j];
        }
    }
    
    int carry = 0;
    for (int i = 0; i < len_a + len_b || carry; i++) {
        if (i < len_a + len_b) carry += c[i];
        c[i] = carry % 10;
        carry /= 10;
    }
    *len_c = len_a + len_b;
    while (*len_c > 0 && c[*len_c - 1] == 0) (*len_c)--;
}


void add(int *a, int len_a, int *b, int len_b, int *res, int *len_res) {
    int i, carry = 0;
    for (i = 0; i < len_a || i < len_b || carry; i++) {
        if (i < len_a) carry += a[i];
        if (i < len_b) carry += b[i];
        res[i] = carry % 10;
        carry /= 10;
    }
    *len_res = i;
}

void subtract(int *a, int len_a, int *b, int len_b, int *res, int *len_res) {
    int i, borrow = 0;
    for (i = 0; i < len_a; i++) {
        res[i] = a[i] - borrow;
        if (i < len_b) res[i] -= b[i];
        if (res[i] < 0) {
            res[i] += 10;
            borrow = 1;
        } else {
            borrow = 0;
        }
    }
    *len_res = len_a;
    while (*len_res > 0 && res[*len_res - 1] == 0) (*len_res)--;
}


void karatsuba_multiply_half(int *a, int *b, int *c, int len_a, int len_b, int *len_c) {
    if (len_a < len_b) {
        karatsuba_multiply_half(b, a, c, len_b, len_a, len_c);
        return;
    }
    if (len_b == 0) {
        *len_c = 0;
        return;
    }
    if (len_a == 1 || len_b == 1) {
        basic_multiply(a, b, c, len_a, len_b, len_c);
        return;
    }
    
    int half = len_a / 2;
    
    int *a0 = a;
    int *a1 = a + half;
    int len_a0 = half;
    int len_a1 = len_a - half;
    
    int *b0 = b;
    int *b1 = b + half;
    int len_b0 = (half < len_b) ? half : len_b;
    int len_b1 = len_b - len_b0;
    
    int *p0 = malloc(2 * half * sizeof(int));
    int *p1 = malloc((len_a + len_b) * sizeof(int));
    int *p2 = malloc((len_a + len_b) * sizeof(int));
    int len_p0, len_p1, len_p2;
    
    karatsuba_multiply_half(a0, b0, p0, len_a0, len_b0, &len_p0);
    karatsuba_multiply_half(a1, b1, p2, len_a1, len_b1, &len_p2);
    
    int *temp1 = malloc((len_a + len_b) * sizeof(int));
    int *temp2 = malloc((len_a + len_b) * sizeof(int));
    int len_temp1 = len_a1, len_temp2 = len_b1;

    add(a0, len_a0, a1, len_a1, temp1, &len_temp1);
    add(b0, len_b0, b1, len_b1, temp2, &len_temp2);

    basic_multiply(temp1, temp2, p1, len_temp1, len_temp2, &len_p1);

    subtract(p1, len_p1, p0, len_p0, p1, &len_p1);
    subtract(p1, len_p1, p2, len_p2, p1, &len_p1);
    
    memset(c, 0, (len_a + len_b) * sizeof(int));
    
    for (int i = 0; i < len_p0; i++) c[i] += p0[i];
    for (int i = 0; i < len_p2; i++) c[i + half * 2] += p2[i];
    for (int i = 0; i < len_p1; i++) c[i + half] += p1[i];

    int carry = 0;
    for (int i = 0; i < len_a + len_b || carry; i++) {
        if (i < len_a + len_b) carry += c[i];
        c[i] = carry % 10;
        carry /= 10;
    }
    *len_c = len_a + len_b;
    while (*len_c > 0 && c[*len_c - 1] == 0) (*len_c)--;
    
    free(p0);
    free(p1);
    free(p2);
    free(temp1);
    free(temp2);
}


void karatsuba_multiply(int *a, int *b, int *c, int len_a, int len_b, int *len_c) {
    if (len_a < len_b) {
        karatsuba_multiply(b, a, c, len_b, len_a, len_c);
        return;
    }
    if (len_b == 0) {
        *len_c = 0;
        return;
    }
    if (len_a == 1) {
        c[0] = a[0] * b[0];
        *len_c = (c[0] > 0) ? 2 : 1;
        c[1] = c[0] / 10;
        c[0] %= 10;
        return;
    }

    int half = len_a / 2;

    int *a0 = a;
    int *a1 = a + half;
    int len_a0 = half;
    int len_a1 = len_a - half;

    int *b0 = b;
    int *b1 = b + half;
    int len_b0 = (half < len_b) ? half : len_b;
    int len_b1 = len_b - len_b0;

    int *p0 = malloc(2 * half * sizeof(int));
    int *p1 = malloc((len_a + len_b) * sizeof(int));
    int *p2 = malloc((len_a + len_b) * sizeof(int));
    int len_p0, len_p1, len_p2;

    karatsuba_multiply(a0, b0, p0, len_a0, len_b0, &len_p0);
    karatsuba_multiply(a1, b1, p2, len_a1, len_b1, &len_p2);

    int *temp1 = malloc((len_a + len_b) * sizeof(int));
    int *temp2 = malloc((len_a + len_b) * sizeof(int));
    int len_temp1 = len_a1, len_temp2 = len_b1;

    add(a0, len_a0, a1, len_a1, temp1, &len_temp1);
    add(b0, len_b0, b1, len_b1, temp2, &len_temp2);

    karatsuba_multiply(temp1, temp2, p1, len_temp1, len_temp2, &len_p1);

    subtract(p1, len_p1, p0, len_p0, p1, &len_p1);
    subtract(p1, len_p1, p2, len_p2, p1, &len_p1);

    memset(c, 0, (len_a + len_b) * sizeof(int));

    for (int i = 0; i < len_p0; i++) c[i] += p0[i];
    for (int i = 0; i < len_p2; i++) c[i + half * 2] += p2[i];
    for (int i = 0; i < len_p1; i++) c[i + half] += p1[i];

    int carry = 0;
    for (int i = 0; i < len_a + len_b || carry; i++) {
        if (i < len_a + len_b) carry += c[i];
        c[i] = carry % 10;
        carry /= 10;
    }
    *len_c = len_a + len_b;
    while (*len_c > 0 && c[*len_c - 1] == 0) (*len_c)--;

    free(p0);
    free(p1);
    free(p2);
    free(temp1);
    free(temp2);
}



int main (int argc, char **argv)
{
    assert (argc == 3);

    char *aStr = argv[1];
    char *bStr = argv[2];

    int len_a = strlen (aStr);
    int len_b = strlen (bStr);
    int len_c = len_a + len_b;

    int *a = malloc (len_a * sizeof (int)); // strings of numbers in reverse order
    int *b = malloc (len_b * sizeof (int)); // strings of numbers in reverse order
    int *c = malloc (len_c * sizeof (int)); // strings of numbers in reverse order

    char *aPtr = strchr (aStr, '\0');
    char *bPtr = strchr (bStr, '\0');
    int i = 0;
    while (aPtr != aStr)
        a[i++] = (*--aPtr) - '0';

    assert (i == len_a);

    i = 0;
    while (bPtr != bStr)
        b[i++] = (*--bPtr) - '0';

    assert (i == len_b);

    for (int i=len_a-1; i>=0; i--)
        printf ("%d",a[i]);

    printf (" * ");

    for (int i=len_b-1; i>=0; i--)
        printf ("%d",b[i]);

    printf (" = ");

    karatsuba_multiply (a,b,c,len_a,len_b,&len_c);
    //karatsuba_multiply_half (a,b,c,len_a,len_b,&len_c);

    int onlyZerosPrinted = 1;
    for (int i=len_c-1; i>=0; i--)
    {
        if (onlyZerosPrinted && c[i] == 0)
            continue;
        onlyZerosPrinted = 0;
        printf ("%d",c[i]);
    }

    printf ("\n");
    return 0;
}
