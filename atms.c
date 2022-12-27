#include <stdio.h>
#include <memory.h>
#include <stdlib.h>
#include <string.h>
/*
∗ CSCI3180 Principles of Programming Languages
∗
∗ --- Declaration ---
∗
∗ I declare that the assignment here submitted is original except for source
∗ material explicitly acknowledged. I also acknowledge that I am aware of
∗ University policy and regulations on honesty in academic work, and of the
∗ disciplinary guidelines and procedures applicable to breaches of such policy
∗ and regulations, as contained in the website
∗ http://www.cuhk.edu.hk/policy/academichonesty/
∗
∗ Assignment 1
∗ Name : Yoo Hyun Jun
∗ Student ID : 1155100531
∗ Email Addr : hjyoo8@cse.cuhk.edu.hk
*/

struct master{
    char acc_name[21];
    char acc_num[17];
    char acc_pwd[7];
    char acc_neg[2];
    int acc_blc[16];
};

typedef struct master master;

int check_acc(char acc_num[], char acc_pwd[]){
    int flag = 0;
    long num;
    master m;
    FILE* fp;
    fp = fopen("master.txt", "r");
    if (fp == NULL){
        printf("fail\n");
        return 0;
    }
    while(1){
        if (feof(fp) != 0) break;
        fgets(m.acc_name, sizeof(m.acc_name),fp);
        fgets(m.acc_num, sizeof(m.acc_num),fp);
        fgets(m.acc_pwd, sizeof(m.acc_pwd),fp);
        fgets(m.acc_neg, sizeof(m.acc_neg),fp);
        fgets(m.acc_blc, sizeof(m.acc_blc),fp);
        if (strcmp(m.acc_num, acc_num) == 0 && strcmp(m.acc_pwd, acc_pwd) == 0){
            flag =1;
            break;
        }
    }
    return flag;
}

int check_blc(char acc_num[], char acc_pwd[]){
    long num;
    int flag = 0;
    master m;
    FILE* fp;
    fp = fopen("master.txt", "r");
    if (fp == NULL){
        printf("fail\n");
        return 0;
    }

    while(1){
    if (feof(fp) != 0) break;
    flag = 0;
    fgets(m.acc_name, sizeof(m.acc_name),fp);
    fgets(m.acc_num, sizeof(m.acc_num),fp);
    fgets(m.acc_pwd, sizeof(m.acc_pwd),fp);
    fgets(m.acc_neg, sizeof(m.acc_neg),fp);
    if (strcmp(m.acc_neg, "-") == 0){
        flag = 1;
    }
    fgets(m.acc_blc, sizeof(m.acc_blc),fp);
    num = atol(m.acc_blc);

    if (strcmp(m.acc_num, acc_num) == 0 && strcmp(m.acc_pwd, acc_pwd) == 0){
        if (flag == 1){
            num = num * -1;
        }
        break;

    }

    }

    return num;
}

int check_tacc(char acc_num[]){
    int flag = 0;
    master m;
    FILE* fp;
    fp = fopen("master.txt", "r");
    if (fp == NULL){
        printf("fail\n");
        return 0;
    }

    while(1){
    if (feof(fp) != 0) break;
    flag = 0;
    fgets(m.acc_name, sizeof(m.acc_name),fp);
    fgets(m.acc_num, sizeof(m.acc_num),fp);
    fgets(m.acc_pwd, sizeof(m.acc_pwd),fp);
    fgets(m.acc_neg, sizeof(m.acc_neg),fp);
    fgets(m.acc_blc, sizeof(m.acc_blc),fp);

    if (strcmp(m.acc_num, acc_num) == 0){
        flag = 1;
        break;

    }

    }

    return flag;
}

void write_atm(int atm_num, char *acc_num[], char *acc_oper[], int amount, int time_stamp){
    FILE *fp;

    if (atm_num == 1){
        fp = fopen("trans711.txt", "a");
        fprintf(fp, acc_num);
        fprintf(fp, acc_oper);
        fprintf(fp, "%07d", amount);
        fprintf(fp, "%05d", time_stamp);
        fputs("\n", fp);
        close(fp);
    }

    else if(atm_num == 2){
        fp = fopen("trans713.txt", "a");
        fprintf(fp, acc_num);
        fprintf(fp, acc_oper);
        fprintf(fp, "%07d", amount);
        fprintf(fp, "%05d", time_stamp);
        fputs("\n", fp);
        close(fp);
    }

    else{
        printf("wrong\n");
        return 0;
    }



}

void write_atm_T(int atm_num, char *acc_num[], char *t_acc_num[], int amount, int time_stamp){
    FILE *fp;

    if (atm_num == 1){
        fp = fopen("trans711.txt", "a");
        fprintf(fp, acc_num);
        fprintf(fp, "W");
        fprintf(fp, "%07d", amount);
        fprintf(fp, "%05d", time_stamp);
        fputs("\n", fp);
        fprintf(fp, t_acc_num);
        fprintf(fp, "D");
        fprintf(fp, "%07d", amount);
        fprintf(fp, "%05d", time_stamp+1);
        fputs("\n", fp);
        close(fp);
    }

    else if(atm_num == 2){
        fp = fopen("trans713.txt", "a");
        fprintf(fp, acc_num);
        fprintf(fp, "W");
        fprintf(fp, "%07d", amount);
        fprintf(fp, "%05d", time_stamp);
        fputs("\n", fp);

        fprintf(fp, t_acc_num);
        fprintf(fp, "D");
        fprintf(fp, "%07d", amount);
        fprintf(fp, "%05d", time_stamp+1);
        fputs("\n", fp);
        close(fp);
    }

    else{
        printf("wrong\n");
        return 0;
    }



}


int main()
{
    int n;
    int time = 0;
    FILE *fp;
    FILE *fp2;
    fp = fopen("trans711.txt", "w");
    fp2 = fopen("trans713.txt", "w");
    fclose(fp);
    fclose(fp2);
    int flag_neg = 0;
    for(int i = 0; i < 46 ; i++){
        printf("#");
    }
    printf("\n");
    printf("##         Gringotts Wizrding Bank          ##\n");
    printf("##                 Welcome                  ##\n");
    printf("##############################################\n");

    while(1){

        while(1){
            if(flag_neg == 1){
                break;
            }
            printf("=> PLEASE CHOOSE THE ATM\n");
            printf("=> PRESS 1 FOR ATM 711\n");
            printf("=> PRESS 2 FOR ATM 713\n");
            scanf("%d", &n);
            if(n == 1 || n == 2){
                break;
            }
            else{
                printf("=> INVALID INPUT\n");
            }
        }

        char acc[21], pwd[7];

        while(1){
            flag_neg = 0;
            printf("=> ACCOUNT\n");
            scanf("%s", &acc);
            printf("=> PASSWORD\n");
            scanf("%s", &pwd);

            if(check_acc(acc,pwd) == 0 ){
                printf("=> INCORRECT ACCOUNT/PASSWORD\n");
            }
            else{
                if (check_blc(acc,pwd) < 0){
                    printf("=> NEGATIVE REMAINS TRANSACTION ABORT\n");
                    flag_neg  = 1;
                    break;

                }
                else{
                    break;
                }


            }

        }

        char service[2];
        float amount;
        char write_amount[8];
        int flag_d, flag_w, flag_t;
        int blc;
        char t_acc_num[17];

        while(1){
            if (flag_neg == 1 ){
                break;
            }
            printf("=> PLEASE CHOOSE YOUR SERVICE\n");
            printf("=> PRESS D FOR DEPOSIT\n");
            printf("=> PRESS W FOR WITHDRAWAL\n");
            printf("=> PRESS T FOR TRANSFER\n");
            scanf("%s", &service);


            if (strcmp(service, "T") == 0){
                    while(1){
                        flag_t = 0;
                        printf("=> TARGET ACCOUNT\n");
                        scanf("%s", &t_acc_num);
                        if (strcmp(t_acc_num, acc) == 0){
                            printf("=> YOO CANNOT TRANSFER TO YOURSELF\n");

                        }
                        else if(check_tacc(t_acc_num) == 0){
                            printf("=> TARGET ACCOUNT DOES NOT EXIST\n");
                        }
                        else{
                            while(1){
                                blc = check_blc(acc, pwd);
                                printf("=> AMOUNT\n");
                                scanf("%f", &amount);
                                amount = amount * 100;
                                if (amount < 0){
                                    printf("=> INVALID INPUT\n");
                                }
                                else if( (blc - amount ) < 0 ){
                                    printf("=> INSUFFICIENT BALANCE\n");
                                }
                                else{
                                    write_atm_T(n, acc, t_acc_num, amount, time);
                                    time+=2;
                                    flag_t = 1;
                                    break;
                                }
                            }
                            if (flag_t == 1){
                                break;
                            }

                        }

                        }
                        if (flag_t == 1){
                                break;
                            }

            }

            else if(strcmp(service, "D") == 0){
                flag_d = 0;
                while(1){
                    printf("=> AMOUNT\n");
                    scanf("%f", &amount);
                    amount = amount * 100;
                    if (amount < 0){
                        printf("=> INVALID INPUT\n");
                    }
                    else{
                        write_atm(n, acc, service, amount, time);
                        time+=1;
                        flag_d = 1;
                        break;
                    }

                }
                if (flag_d == 1){
                    break;
                }

            }
            else if(strcmp(service, "W") == 0){
                blc = check_blc(acc, pwd);
                flag_w = 0;
                while(1){
                printf("=> AMOUNT\n");
                scanf("%f", &amount);
                amount = amount * 100;
                if (amount < 0){
                    printf("=> INVALID INPUT\n");
                }
                else if( (blc - amount ) < 0 ){
                    printf("=> INSUFFICIENT BALANCE\n");
                }
                else{
                    write_atm(n, acc, service, amount, time);
                    time+=1;
                    flag_w = 1;
                    break;
                }
                }

                if (flag_w == 1){
                    break;
                }

            }

            else{
                    printf("=> INVALID INPUT\n");
            }
        }

        char end[2];
        int end_program = 0;
        while(1){
            if (flag_neg == 1 ){
                break;
            }
            printf("=> CONTINUE?\n");
            printf("=> N FOR NO\n");
            printf("=> Y FOR YES\n");
            scanf("%s", &end);
            if (strcmp(end, "Y") == 0){
                end_program = 0;
                break;
            }
            else if(strcmp(end, "N") == 0){
                end_program = 1;
                break;
            }
            else{
                printf("=> INVALID INPUT\n");
            }

        }
        if(end_program == 1){
            break;
        }
        else{
            continue;
        }

    }





    return 0;
}

