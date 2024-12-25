#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>

typedef struct s_list {
    int num;
    struct s_list* next;
} t_list;

t_list* create_node(int set_num) {
    t_list* node = (t_list*)malloc(sizeof(t_list));
    node->num = set_num;
    node->next = NULL;
    return node;
}

void push_back(t_list** list, int set_num) {
    t_list* new_element = create_node(set_num);
    t_list* tmp = *list;
    if (tmp == NULL) {
        *list = new_element;
        return;
    }
    while (tmp->next != NULL) {
        tmp = tmp->next;
    }
    tmp->next = new_element;
}

void print_list(t_list** list) {
    fflush(stdout);
    t_list* tmp_out = *list;
    while (tmp_out != NULL) {
        printf("%d ", tmp_out->num);
        tmp_out = tmp_out->next;
    }
}

int int_check(void) {
    int int_num = 0, checker;
    char last;
    while (checker != 2 || last != '\n') {
        checker = scanf("%d%c", &int_num, &last);
        fflush(stdin);
        if (checker != 2 || last != '\n') printf("You inputed not integer number.\n");
    }
    return int_num;
}

void result(t_list** list_res, t_list** list_res_1, int a, int b) {
    t_list* tmp_res_1 = *list_res_1;
    t_list* tmp_res = *list_res;
    while (tmp_res_1 != NULL) {
        if (tmp_res_1->num >= a && tmp_res_1->num <= b) {
            push_back(&tmp_res, tmp_res_1->num);
        }
        else {
            continue;
        }
        tmp_res_1 = tmp_res_1->next;
    }
}

int main() {
    while (true) {
        char start_input[] = "1";
        char user_input[20];
        int user_input_a;
        int user_input_b;

        printf("\nPrint 1 to start ");
        scanf("%s", user_input);
        printf("\n");
        fflush(stdin);

        if (strcmp(start_input, user_input) == 0) {
            int tmp_num;
            printf("\nTo exit Print 0 ");
            printf("\nElement 1 :");
            tmp_num = int_check();
            t_list* list = create_node(tmp_num);
            int counter = 1;
            while (tmp_num != 0) {
                counter++;
                printf("\nElement %d ", counter);
                tmp_num = int_check();
                if (tmp_num == 0) {
                    counter--;
                    break;
                }
                push_back(&list, tmp_num);
            }


        printf("\nYour list : ");
        print_list(&list);
        fflush(stdin);
        fflush(stdout);




        printf("\nWrite lower border of the interval : ");
        user_input_a = int_check();
        int a=user_input_a;
        printf("\nWrite upper border of the interval : ");
        user_input_b = int_check();
        int b=user_input_b;


        t_list *listrange = create_node(user_input_a);
        while(user_input_a!=user_input_b){
          user_input_a++;
          push_back(&listrange,user_input_a);
        }

        printf("\nYour list range : ");
        print_list(&listrange);
        }else{
            break;
        }

    }

} 