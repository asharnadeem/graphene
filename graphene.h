#pragma once

struct l_node
{
    void *element;
    struct list *next;
};

struct list {
    int size;
    struct l_node *head;
};

int index(struct list *list, int index);

int add_tail(struct list *list, void *element);