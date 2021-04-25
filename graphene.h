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

struct graph
{   // struct list size makes this redundant
    // int size;
    struct list *nodes;
    struct node *root;
};

void printbig(int c);

struct list *list_init();
void *list_index(struct list *list, int index);
void list_push_back(struct list *list, void *element);
void list_push_front(struct list *list, void *element);
void *list_pop_back(struct list *list);
void *list_pop_front(struct list *list);
void *list_peek_back(struct list *list);
void *list_peek_front(struct list *list);
int list_size(struct list *list);
int list_empty(struct list *list);

struct node *node_init();
void node_set_id(struct node *node, int id);
void node_set_val(struct node *node, void *val);

struct edge *edge_init(int w, struct node *n, int tr);

struct graph *graph_init();
int graph_size(struct graph *graph);
void graph_add_node(struct graph *graph, struct node *node);
struct node *graph_add(struct graph *graph, int id, void *val);
struct node *graph_get_node(struct graph *graph, int key);
int graph_contains_node(struct graph *graph, struct node *node);
int graph_contains_id(struct graph *graph, int id);

int string_cmp(void *v1, void*v2);
static const char font[] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x1c, 0x3e, 0x61, 0x41, 0x43, 0x3e, 0x1c, 0x00,
    0x00, 0x40, 0x42, 0x7f, 0x7f, 0x40, 0x40, 0x00,
    0x62, 0x73, 0x79, 0x59, 0x5d, 0x4f, 0x46, 0x00,
    0x20, 0x61, 0x49, 0x4d, 0x4f, 0x7b, 0x31, 0x00,
    0x18, 0x1c, 0x16, 0x13, 0x7f, 0x7f, 0x10, 0x00,
    0x27, 0x67, 0x45, 0x45, 0x45, 0x7d, 0x38, 0x00,
    0x3c, 0x7e, 0x4b, 0x49, 0x49, 0x79, 0x30, 0x00,
    0x03, 0x03, 0x71, 0x79, 0x0d, 0x07, 0x03, 0x00,
    0x36, 0x4f, 0x4d, 0x59, 0x59, 0x76, 0x30, 0x00,
    0x06, 0x4f, 0x49, 0x49, 0x69, 0x3f, 0x1e, 0x00,
    0x7c, 0x7e, 0x13, 0x11, 0x13, 0x7e, 0x7c, 0x00,
    0x7f, 0x7f, 0x49, 0x49, 0x49, 0x7f, 0x36, 0x00,
    0x1c, 0x3e, 0x63, 0x41, 0x41, 0x63, 0x22, 0x00,
    0x7f, 0x7f, 0x41, 0x41, 0x63, 0x3e, 0x1c, 0x00,
    0x00, 0x7f, 0x7f, 0x49, 0x49, 0x49, 0x41, 0x00,
    0x7f, 0x7f, 0x09, 0x09, 0x09, 0x09, 0x01, 0x00,
    0x1c, 0x3e, 0x63, 0x41, 0x49, 0x79, 0x79, 0x00,
    0x7f, 0x7f, 0x08, 0x08, 0x08, 0x7f, 0x7f, 0x00,
    0x00, 0x41, 0x41, 0x7f, 0x7f, 0x41, 0x41, 0x00,
    0x20, 0x60, 0x40, 0x40, 0x40, 0x7f, 0x3f, 0x00,
    0x7f, 0x7f, 0x18, 0x3c, 0x76, 0x63, 0x41, 0x00,
    0x00, 0x7f, 0x7f, 0x40, 0x40, 0x40, 0x40, 0x00,
    0x7f, 0x7f, 0x0e, 0x1c, 0x0e, 0x7f, 0x7f, 0x00,
    0x7f, 0x7f, 0x0e, 0x1c, 0x38, 0x7f, 0x7f, 0x00,
    0x3e, 0x7f, 0x41, 0x41, 0x41, 0x7f, 0x3e, 0x00,
    0x7f, 0x7f, 0x11, 0x11, 0x11, 0x1f, 0x0e, 0x00,
    0x3e, 0x7f, 0x41, 0x51, 0x71, 0x3f, 0x5e, 0x00,
    0x7f, 0x7f, 0x11, 0x31, 0x79, 0x6f, 0x4e, 0x00,
    0x26, 0x6f, 0x49, 0x49, 0x4b, 0x7a, 0x30, 0x00,
    0x00, 0x01, 0x01, 0x7f, 0x7f, 0x01, 0x01, 0x00,
    0x3f, 0x7f, 0x40, 0x40, 0x40, 0x7f, 0x3f, 0x00,
    0x0f, 0x1f, 0x38, 0x70, 0x38, 0x1f, 0x0f, 0x00,
    0x1f, 0x7f, 0x38, 0x1c, 0x38, 0x7f, 0x1f, 0x00,
    0x63, 0x77, 0x3e, 0x1c, 0x3e, 0x77, 0x63, 0x00,
    0x00, 0x03, 0x0f, 0x78, 0x78, 0x0f, 0x03, 0x00,
    0x61, 0x71, 0x79, 0x5d, 0x4f, 0x47, 0x43, 0x00};