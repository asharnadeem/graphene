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

int add_tail(struct list *list, void *element)
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