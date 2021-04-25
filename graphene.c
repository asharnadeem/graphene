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
       list->size = list->size + 1;
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
        list->size = list->size - 1;
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
        list->size = list->size - 1;
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
    return NULL;
}

void *list_peek_back(struct list *list) {
  struct list_element *tmp = list->head;
  while(tmp->next != NULL) {
    tmp = tmp -> next;
  }
  return tmp->element;
}
void *list_peek_front(struct list *list) {
  return list->head->element;
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
    //node->val = malloc(sizeof(void *));
    node->edges = malloc(sizeof(struct list));
    return node;
}

// void node_set_id(struct node *node, int id)
// {
//     node->id = id;
// }

// void node_set_val(struct node *node, void *val)
// {
//     node->val = val;
// }

// int node_get_id(struct node *node)
// {
//     return node->id;
// }

// void node_get_val(struct node *node)
// {
//     return *node->val;
// }

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

void edge_check(struct node *n1, struct node *n2) {

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
    //  printf("addnode\n");
    //  printf("adding: %d, %d", node->id, *(int  *)(node->val));
    if (graph->root == NULL)
    {
        graph->root = node;
    }

    struct list_element *tmp;
    tmp = graph->nodes->head;
    if(tmp == NULL) 
    {
      // printf("HEAD NULL\n");
      struct list_element* newel = (struct list_element*) malloc(sizeof(struct list_element));
      newel->element = node;
      newel->next = NULL;
      graph->nodes->head = newel;
    }
    else {
      while (tmp->next != NULL)
      {   
          // printf("on: %d, adding: %d\n", node->id,  ((struct node *)tmp->element)->id);
          if(node->id == ((struct node *)tmp->element)->id) 
          {
            
            // printf("graph already has node of id = %d\n", node->id);
          }
          tmp = tmp->next;
      }
      // printf("on: %d, adding: %d\n", node->id, ((struct node *)tmp->element)->id );
      if(node->id == ((struct node *)tmp->element)->id) 
      {
        // printf("graph already has node of id = %d\n", node->id);
      }

      struct list_element* newel = (struct list_element*) malloc(sizeof(struct list_element));
      newel->element = (void *) node;
      newel->next = NULL;
      tmp->next = newel;
    
    // printf("added: %d, %d\n", ((struct node *) newel->element)-> id, *(int *)(((struct node *) newel->element)->val));
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
    // if (graph->head == NULL)
    // {
    //     return NULL;
    // }
    //printf("searching for node with key: %d\n", id);

    struct list_element *tmp;
    tmp = graph->nodes->head;
    // if(tmp == NULL) {
    //   return NULL;
    // }
    // printf("1\n");

    while (tmp != NULL) 
    {
        //printf("looping, cur id = %d\n", ((struct node *) tmp->element)->id);
        //printf("2\n");
        if(((struct node *) tmp->element)->id == id)
        {
            //printf("NODE FOUND\n");
            return (struct node *) tmp->element;
        }
        tmp = tmp->next;
    }
    //printf("node not found\n");
    //printf("3\n");
    return NULL;
    
}
int graph_contains_node(struct graph *graph, struct node *node) {
  struct list_element *tmp = graph->nodes->head;
  while (tmp != NULL) {
    if(tmp->element == node) {
      return 1;
    }
    tmp = tmp->next;
  }
  return 0;
}

int graph_contains_id(struct graph *graph, int id) {
  return graph_get_node(graph, id) != NULL;
}

/* ---------- End Graph Functions ---------- */


/* ---------- Misc. Functions ---------- */

int string_cmp(void *v1, void *v2) {
    char *s1 = (char *) v1;
    char *s2 = (char *) v2;
    while(*s1 && *s2) {
        if(*s1++ == *s2++) { }
        else {
            return 0;
        }
    }
    if(*s1 == *s2) {
        return 1;
    }
    else {
        return 0;
    }
}
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

//  int main()
//  {
//     struct graph *g = graph_init();
//     struct node *n = node_init();
//     n->id = 10;
//     n->val = malloc(sizeof(int));
//     struct node *m = node_init();
//     m->id = 0;
//     m->val = malloc(sizeof(int));
//     graph_add_node(g, n);
//     graph_add_node(g, m);
//     struct list *l = g->nodes;
//     printf("%d\n", ((struct node *)(list_index(l, 1)))->id);


//  }
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