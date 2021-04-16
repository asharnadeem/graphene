#pragma once

struct l_node
{
    int element;
    struct l_node *next;
};

struct node
{
    int id;
    void *val;
};



struct list {
    int size;
    struct l_node *head;
};

void printbig(int c);

struct list * list_init();

struct node * node_init();

void node_idset(struct node *node, int id);

void node_valset(struct node *node, void *val);

int list_index(struct list *list, int index);

int list_push_back(struct list *list, int element);
