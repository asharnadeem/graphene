#pragma once

struct l_node
{
    void *element;
    struct l_node *next;
};

struct list {
    int size;
    struct l_node *head;
};

struct edge {
    int weight;
    struct node *node;
    int t;
};

struct node
{
    int id;
    void *val;
    struct list *edges;
};

void printbig(int c);

struct list * list_init();

struct node * node_init();

void node_idset(struct node *node, int id);

void node_valset(struct node *node, void *val);

void *list_index(struct list *list, int index);

int list_push_back(struct list *list, void *element);

struct edge * edge_init(int w, struct node * n, int tr);
