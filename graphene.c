#include <stdlib.h>
#include <stdio.h>
#include "graphene.h"

int index(struct list *list, int index)
{
    int cur = 0;

    struct l_node *tmp = list->head;
    while (tmp->next != NULL)
    {
        if(cur == index)
        {
            return *(int *) tmp->element;
        }
        else
        {
            tmp = tmp->next;
            cur++;
        }
    } 
    return -1;
}

int push_back_int(struct list *list, int element)
{

    struct l_node *node = (struct l_node *)malloc(sizeof(struct l_node));

    node->next = NULL;
    node->element = element;

    if (list->head == NULL)
    {
        list->size = 1;
        list->head = node;
        return 0;
    }

    struct l_node *tmp = list->head;
    while (tmp->next != NULL)
    {
        tmp = tmp->next;
    }

    tmp->next = node;
    list->size = list->size + 1;
    return 0;
}

int main()
{
    
}

#ifdef BUILD_TEST
int main()
{
  char s[] = "HELLO WORLD09AZ";
  char *c;
  for ( c = s ; *c ; c++) printf(".");
}
#endif

// int main(){
    
//     struct list *test = (struct list *)malloc(sizeof(struct list));
//     struct l_node *node = (struct l_node *)malloc(sizeof(struct l_node));
//     node->element = 10;
//     node->next = NULL;
//     test->head = node;
//     test->size = 1;
//     push_back(test, 5);
    
//     struct l_node *node2 = (struct l_node *)malloc(sizeof(struct l_node));
//     node2 = test->head->next;

//     printf("%d\n", node2->element);
// }
