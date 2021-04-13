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

int index(struct list *list, int index);

int push_back_int(struct list *list, int element);