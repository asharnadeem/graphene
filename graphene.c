/* functions implementing Graphene build-in functionality
    Author: Ashar Nadeem & Matthew Sanchez */
#include <stdlib.h>
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

    node->element = element;
    node->next = NULL;

    if (list->head == NULL)
    {
        list->head = node;
        list->size = list->size + 1;
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
    node->element = element;
    node->next = (list->head);
    list->head = node;
    list->size = list->size + 1;
}

void *list_pop_back(struct list *list)
{
    struct list_element *tmp = list->head;
    struct list_element *t;
    if (list->head->next == NULL)
    {
        void *data = list->head->element;
        free(list->head);
        list->head = NULL;
        list->size = list->size - 1;
        return data;
    }
    else
    {
        while (tmp->next != NULL)
        {
            t = tmp;
            tmp = tmp->next;
        }
        void *data = t->next->element;
        free(t->next);
        t->next = NULL;
        list->size = list->size - 1;
        return data;
    }
}

void *list_pop_front(struct list *list)
{
    if (list->head != NULL)
    {
        void *data = list->head->element;
        struct list_element *tmp = list->head->next;
        free(list->head);
        list->head = tmp;
        list->size = list->size - 1;
        return data;
    }
    return NULL;
}

void *list_peek_back(struct list *list)
{
    struct list_element *tmp = list->head;
    while (tmp->next != NULL)
    {
        tmp = tmp->next;
    }
    return tmp->element;
}
void *list_peek_front(struct list *list)
{
    return list->head->element;
}


/* ---------- End List Functions ---------- */

/* ---------- Node Functions ---------- */

struct node *node_init()
{
    struct node *node = malloc(sizeof(struct node));
    //node->val = malloc(sizeof(void *));
    node->edges = malloc(sizeof(struct list));
    return node;
}

/* ---------- End Node Functions ---------- */

/* ---------- Edge Functions ----------*/

struct edge *edge_init(void *w, struct node *n, int tr)
{
    struct edge *edge = malloc(sizeof(struct edge));
    edge->weight = w;
    edge->dest = n;
    edge->t = tr;
    return edge;
}

void edge_check(struct node *n1, struct node *n2)
{
}
/* ---------- End Edge Functions ---------- */

/* ---------- Graph Functions ---------- */

struct graph *graph_init()
{
    struct graph *graph = malloc(sizeof(struct graph));
    graph->nodes = list_init();
    graph->root = NULL;
    return graph;
}

void graph_add_node(struct graph *graph, struct node *node)
{
    if (graph->root == NULL)
    {
        graph->root = node;
    }

    struct list_element *tmp;
    tmp = graph->nodes->head;
    if (tmp == NULL)
    {
      struct list_element* newel = (struct list_element*) malloc(sizeof(struct list_element));
      newel->element = node;
      newel->next = NULL;
      graph->nodes->head = newel;
    }
    else {
      while (tmp->next != NULL)
      {   
          tmp = tmp->next;
      }

      struct list_element* newel = (struct list_element*) malloc(sizeof(struct list_element));
      newel->element = (void *) node;
      newel->next = NULL;
      tmp->next = newel;
    

    }
    graph->nodes->size = graph->nodes->size + 1;
}

struct node *graph_add(struct graph *graph, int id, void *val)
{
    struct node *node = node_init();
    node->id = id;
    node->val = val;
    graph_add_node(graph, node);
    return node;
}

struct node *graph_get_node(struct graph *graph, int id)
{
    struct list_element *tmp;
    tmp = graph->nodes->head;

    while (tmp != NULL)
    {
        if(((struct node *) tmp->element)->id == id)
        {
            return (struct node *) tmp->element;
        }
        tmp = tmp->next;
    }
    return NULL;
}
int graph_contains_node(struct graph *graph, struct node *node)
{
    struct list_element *tmp = graph->nodes->head;
    while (tmp != NULL)
    {
        if (tmp->element == node)
        {
            return 1;
        }
        tmp = tmp->next;
    }
    return 0;
}

int graph_contains_id(struct graph *graph, int id)
{
    return graph_get_node(graph, id) != NULL;
}

/* ---------- End Graph Functions ---------- */

/* ---------- Misc. Functions ---------- */

int string_cmp(void *v1, void *v2)
{
    char *s1 = (char *)v1;
    char *s2 = (char *)v2;
    while (*s1 && *s2)
    {
        if (*s1++ == *s2++)
        {
        }
        else
        {
            return 0;
        }
    }
    if (*s1 == *s2)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}