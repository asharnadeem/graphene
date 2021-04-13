#pragma once

struct l_node
{
    int element;
    struct list *next;
};

struct list {
    int size;
    struct l_node *head;
};

void printbig(int c);

struct list * list_init();

int list_index(struct list *list, int index);

int list_push_back(struct list *list, int element);
