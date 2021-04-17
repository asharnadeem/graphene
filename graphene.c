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
    struct list_element *node = (struct list_element *)malloc(sizeof(struct list_element));

    node->next = NULL;
    node->element = element;

    if (list->head == NULL)
    {
        list->size = 1;
        list->head = node;
        return;
    }

    struct list_element *tmp = list->head;

    while (tmp->next != NULL)
    {
        tmp = tmp->next;
    }

    tmp->next = node;
    list->size = list->size + 1;
}

void list_push_front(struct list *list, void *element)
{

    struct list_element *node = (struct list_element *)malloc(sizeof(struct list_element));

    node->next = NULL;
    node->element = element;

    if (list->head == NULL)
    {
        list->size = 1;
        list->head = node;
        return;
    }
}

int list_size(struct list *list)
{
    return list->size;
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
    tmp->next = graph->head;

    while (tmp->next != NULL)
    {
        tmp = tmp->next;
    }

    tmp->next = node;
    graph->size = graph->size + 1;
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