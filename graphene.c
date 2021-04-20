#include <stdlib.h>
#include <stdio.h>
#include "graphene.h"

/* ---------- List Functions ---------- */

struct list *list_init()
{
    struct list *list = malloc(sizeof(struct list));
    list->size = 0;
    list->head = NULL;
    return list;
}

void *list_index(struct list *list, int index)
{
    int cur = 0;

    struct list_element *tmp = list->head;
    while (tmp != NULL)
    {
        if (cur == index)
        {
            return tmp->element;
        }
        else
        {
            tmp = tmp->next;
            cur++;
        }
    }
    return 0;
}

void list_push_back(struct list *list, void *element)
{
    struct list_element* node = (struct list_element*) malloc(sizeof(struct list_element));
   
    node->element  = element;
    node->next = NULL;
  
    if (list->head == NULL)
    {
       list->head = node;
       return;
    }  
       
    struct list_element* tmp = list->head;
    while (tmp->next != NULL){
        tmp = tmp->next;
    }
   
    tmp->next = node;
    list->size = list->size + 1;
}

void list_push_front(struct list *list, void *element)
{
    struct list_element* node = (struct list_element*) malloc(sizeof(struct list_element));
    node->element  = element;
    node->next = (list->head);
    list->head = node;
    list->size = list->size + 1;
}

void *list_pop_back(struct list *list)
{
    struct list_element *tmp = list->head;
    struct list_element *t;
    if(list->head->next==NULL)
    {
        void *data = list->head->element;
        free(list->head);
        list->head = NULL;
        return data;
    }
    else
    {
        while(tmp->next != NULL)
        {
            t = tmp;
            tmp = tmp->next;
        }
        void *data = t->next->element;
        free(t->next);
        t->next=NULL; 
        return data;
    }    
}

void *list_pop_front(struct list *list)
{
    if(list->head != NULL) {
        void *data = list->head->element;
        struct list_element *tmp = list->head->next;
        free(list->head);
        list->head = tmp;
        list->size = list->size - 1;
        return data;
    }
}

int list_size(struct list *list)
{
    return list->size;
}

int list_empty(struct list *list)
{
    if(list->size == 0)
    {
        return 1;
    }
    return 0;
}

/* ---------- End List Functions ---------- */


/* ---------- Node Functions ---------- */

struct node *node_init()
{
    struct node *node = malloc(sizeof(struct node));
    node->val = malloc(sizeof(void *));
    node->edges = malloc(sizeof(struct list *));
    node->next = NULL;
    return node;
}

void node_set_id(struct node *node, int id)
{
    node->id = id;
}

void node_set_val(struct node *node, void *val)
{
    node->val = val;
}

int node_get_id(struct node *node)
{
    return node->id;
}

void node_get_val(struct node *node)
{
    return *node->val;
}

/* ---------- End Node Functions ---------- */


/* ---------- Edge Functions ----------*/

struct edge *edge_init(int w, struct node *n, int tr)
{
    struct edge *edge = malloc(sizeof(struct edge));
    edge->weight = w;
    edge->node = n;
    edge->t = tr;
    return edge;
}

/* ---------- End Edge Functions ---------- */


/* ---------- Graph Functions ---------- */

struct graph *graph_init()
{
    struct graph *graph = malloc(sizeof(struct graph));
    graph->size = 0;
    graph->head = NULL;
    return graph;
}

int graph_size(struct graph *graph)
{
    return graph->size;
}

void graph_add_node(struct graph *graph, struct node *node)
{
    if (graph->head == NULL)
    {
        graph->size = 1;
        graph->head = node;
        return ;
    }

    struct node *tmp = node_init();
    tmp = graph->head;

    while (tmp->next != NULL)
    {
        tmp = tmp->next;
    }

    tmp->next = node;
    graph->size = graph->size + 1;
}

struct node *graph_get_node(struct graph *graph, int key)
{
    if (graph->head == NULL)
    {
        struct node *null = node_init();
        return null;
    }

    struct node *tmp = node_init();
    tmp = graph->head;

    do
    {
        if(tmp->id == key){
            return tmp;
        }
        tmp = tmp->next;
    }
    while (tmp->next != NULL);
}

/* ---------- End Graph Functions ---------- */


/* ---------- Misc. Functions ---------- */

void printbig(int c)
{
    int index = 0;
    int col, data;
    if (c >= '0' && c <= '9')
        index = 8 + (c - '0') * 8;
    else if (c >= 'A' && c <= 'Z')
        index = 88 + (c - 'A') * 8;
    do
    {
        data = font[index++];
        for (col = 0; col < 8; data <<= 1, col++)
        {
            char d = data & 0x80 ? 'X' : ' ';
            putchar(d);
            putchar(d);
        }
        putchar('\n');
    } while (index & 0x7);
}

// int main()
// {
//     int a = 1;
//     int b = 2;

//     struct list *l = malloc(sizeof(struct list));
//     list_push_back(l, &a);
//     list_push_back(l, &b);

//     int c =  *( (int *) list_pop_back(l));
//     int d =  *( (int *) list_pop_back(l));
//     // int e =  *( (int *) list_pop_front(l));

//     printf("%d\n", c);
//     printf("%d\n", d);
//     // /printf("%d\n", e);
// }