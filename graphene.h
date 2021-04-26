/* header file for Graphene build-in functionality
    Author: Ashar Nadeem & Matthew Sanchez */
#pragma once

struct list_element
{
    void *element;
    struct list_element *next;
};

struct list {
    int size;
    struct list_element *head;
};

struct edge {
    void *weight;
    struct node *dest;
    int t;
};

struct node
{
    int id;
    void *val;
    struct list *edges;
};

struct graph
{   
    struct list *nodes;
    struct node *root;
};

struct list *list_init();
void *list_index(struct list *list, int index);
void list_push_back(struct list *list, void *element);
void list_push_front(struct list *list, void *element);
void *list_pop_back(struct list *list);
void *list_pop_front(struct list *list);
void *list_peek_back(struct list *list);
void *list_peek_front(struct list *list);

struct node *node_init();
void node_set_id(struct node *node, int id);
void node_set_val(struct node *node, void *val);

struct edge *edge_init(void *w, struct node *n, int tr);

struct graph *graph_init();
int graph_size(struct graph *graph);
void graph_add_node(struct graph *graph, struct node *node);
struct node *graph_add(struct graph *graph, int id, void *val);
struct node *graph_get_node(struct graph *graph, int key);
int graph_contains_node(struct graph *graph, struct node *node);
int graph_contains_id(struct graph *graph, int id);

int string_cmp(void *v1, void*v2);