#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

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

////////////////////////////////////////////////
// global variable
int MAX_TRAN = 10;

struct transaction {
    char transac_account[20];
    char neg[2];
    char others[14];
    char timestamp[10];
};

struct master{
    char acc_name[21];
    char acc_num[17];
    char acc_pwd[7];
    char acc_neg[2];
    char acc_blc[16];
};

void swap(struct transaction *a , struct transaction *b)
{
    struct transaction temp ;
    temp = *a ;
    *a = *b ;
    *b = temp ;
    return ;
}
struct transaction ** sort_transactions(struct transaction ** transaction_i){

    int transaction_index = 0, temp_index = 0;
    while (transaction_i[transaction_index] != NULL) {
        temp_index = transaction_index;
        while (transaction_i[temp_index] != NULL){
            if (strcmp(transaction_i[temp_index]->transac_account,transaction_i[transaction_index]->transac_account)<0){
                swap(transaction_i[temp_index], transaction_i[transaction_index]);
            }
            else{
                if (strcmp(transaction_i[temp_index]->transac_account,transaction_i[transaction_index]->transac_account)==0){
                    if (strcmp(transaction_i[temp_index]->timestamp,transaction_i[transaction_index]->timestamp)<0){
                        swap(transaction_i[temp_index], transaction_i[transaction_index]);
                    }
                }
            }
            temp_index += 1;

        }
        transaction_index += 1;
    }
    return transaction_i;
}
void read_str(char input_line[], char output_line[], int start_index, int length) {
    strncpy(output_line, input_line + start_index, length);
    output_line[length] = '\0';
}

struct transaction * process_one_transaction(char line[]) {
    struct transaction * result_transaction = (struct transaction *) malloc(sizeof(struct transaction));

    char temp_str[30];

    // transaction account
    read_str(line, temp_str, 0, 16);
    strcpy(result_transaction->transac_account , temp_str);

    // operation
    read_str(line, temp_str, 16, 1);
    strcpy(result_transaction->neg , temp_str);

    read_str(line, temp_str, 17,7);
    strcpy(result_transaction->others , temp_str);
    // timestamp
    read_str(line, temp_str, 24, 5);
    strcpy(result_transaction->timestamp , temp_str);

    return result_transaction;
}

void save_transactions(struct transaction ** transactions, char save_path[]){
    FILE * fp = fopen(save_path, "w");
    int transaction_index = 0;
    while(transactions[transaction_index] != NULL){
        fprintf(fp, "%.16s%.1s%.7s%.5s", transactions[transaction_index]->transac_account, transactions[transaction_index]->neg, transactions[transaction_index]->others, transactions[transaction_index]->timestamp);
        fprintf(fp, "%s", "\n");
        transaction_index += 1;
    }
    fclose(fp);
}

struct transaction ** get_transactions(char stat_path[]){
    struct transaction ** all_transactions = (struct transaction **) malloc(sizeof(struct transaction *) * MAX_TRAN);
    FILE * fp = fopen(stat_path, "r");
    for (int i = 0; i < MAX_TRAN; i++) all_transactions[i] = NULL;
    char line[60];
    int cnt = 0;
    while (fgets(line, 60, fp) != NULL) {
        all_transactions[cnt] = process_one_transaction(line);
        cnt += 1;

        if (cnt == MAX_TRAN) {
            // allocate more space
            MAX_TRAN *= 2;
            struct transaction ** all_transactions_temp;
            all_transactions_temp = realloc(all_transactions, sizeof(struct transaction *) * MAX_TRAN);
            all_transactions = all_transactions_temp;
        }
    }
    return all_transactions;

}

struct master * read_master(char line[]){
    struct master * result_master = (struct master *) malloc(sizeof(struct master));
    char temp_str[30];
    //account name
    read_str(line, temp_str,0,20);
    strcpy(result_master->acc_name, temp_str);
    //account num
    read_str(line, temp_str, 20, 16);
    strcpy(result_master->acc_num, temp_str);
    //account pwd
    read_str(line, temp_str, 36, 6);
    strcpy(result_master->acc_pwd, temp_str);
    //account sign
    read_str(line, temp_str, 42, 1);
    strcpy(result_master->acc_neg, temp_str);
    //account balance
    read_str(line, temp_str, 43, 16);
    strcpy(result_master->acc_blc, temp_str);
    return result_master;

};

struct master ** get_master_transactions(char stat_path[]){
    struct master ** all_master = (struct master **) malloc(sizeof(struct master *) * MAX_TRAN);
    FILE * fp = fopen(stat_path, "r");
    for (int i = 0; i < MAX_TRAN; i++) all_master[i] = NULL;
    char line[60];
    int cnt = 0;
    while (fgets(line, 60, fp) != NULL) {
        all_master[cnt] = read_master(line);
        cnt += 1;

        if (cnt == MAX_TRAN) {
            // allocate more space
            MAX_TRAN *= 2;
            struct master ** all_master_temp;
            all_master_temp = realloc(all_master, sizeof(struct master *) * MAX_TRAN);
            all_master = all_master_temp;
        }
    }
    return all_master;

}


void merge_transactions(struct transaction ** atm_711, struct transaction ** atm_713, char save_path[]){
    FILE * fp = fopen(save_path, "w");
    int atm_711_index = 0;
    int atm_713_index = 0;

    while(1){
        if( (atm_711[atm_711_index] == NULL) && ( atm_713[atm_713_index] == NULL )){
            break;
        }
        if (atm_711[atm_711_index]->transac_account != NULL && atm_713[atm_713_index]->transac_account != NULL){
            if (strcmp(atm_711[atm_711_index]->transac_account, atm_713[atm_713_index]->transac_account) < 0){ //acc_num_711 < acc_num_713
            fprintf(fp, "%.16s%.1s%.7s%.5s", atm_711[atm_711_index]->transac_account, atm_711[atm_711_index]->neg, atm_711[atm_711_index]->others, atm_711[atm_711_index]->timestamp);
            fprintf(fp, "%s", "\n");
            atm_711_index += 1;
        }
        else if(strcmp(atm_711[atm_711_index]->transac_account, atm_713[atm_713_index]->transac_account) == 0){ //acc_num_711 = acc_num_713

                    if (strcmp(atm_711[atm_711_index]->timestamp,atm_713[atm_713_index]->timestamp) <0){ //acc_timestamp_711 < acc_timestamp_713
                        fprintf(fp, "%.16s%.1s%.7s%.5s", atm_711[atm_711_index]->transac_account, atm_711[atm_711_index]->neg, atm_711[atm_711_index]->others, atm_711[atm_711_index]->timestamp);
                        fprintf(fp, "%s", "\n");
                        atm_711_index += 1;
                    }
                    else{ //acc_timestamp_711 > acc_timestamp_713
                        fprintf(fp, "%.16s%.1s%.7s%.5s", atm_713[atm_713_index]->transac_account, atm_713[atm_713_index]->neg, atm_713[atm_713_index]->others, atm_713[atm_713_index]->timestamp);
                        fprintf(fp, "%s", "\n");
                        atm_713_index += 1;
                    }

        }
        else if (strcmp(atm_711[atm_711_index]->transac_account, atm_713[atm_713_index]->transac_account) > 0){ //acc_num_711 > acc_num_713
            fprintf(fp, "%.16s%.1s%.7s%.5s", atm_713[atm_713_index]->transac_account, atm_713[atm_713_index]->neg, atm_713[atm_713_index]->others, atm_713[atm_713_index]->timestamp);
            fprintf(fp, "%s", "\n");
            atm_713_index += 1;
        }

        }
        else if(atm_711[atm_711_index]->transac_account == NULL && atm_713[atm_713_index]->transac_account != NULL){ //only acc_num_713
            fprintf(fp, "%.16s%.1s%.7s%.5s", atm_713[atm_713_index]->transac_account, atm_713[atm_713_index]->neg, atm_713[atm_713_index]->others, atm_713[atm_713_index]->timestamp);
            fprintf(fp, "%s", "\n");
            atm_713_index += 1;
        }
        else if(atm_711[atm_711_index]->transac_account != NULL && atm_713[atm_713_index]->transac_account == NULL){ //only acc_num_711
            fprintf(fp, "%.16s%.1s%.7s%.5s", atm_711[atm_711_index]->transac_account, atm_711[atm_711_index]->neg, atm_711[atm_711_index]->others, atm_711[atm_711_index]->timestamp);
            fprintf(fp, "%s", "\n");
            atm_711_index += 1;
        }
    }
    fclose(fp);
}

void upd_master(struct transaction ** merge_sorted, struct master ** master_before, char save_path[]){
    FILE * fp = fopen(save_path, "w");
    int master_index= 0 ;
    int merge_index = 0 ;
    int blc = atoi(master_before[master_index]->acc_blc);
    int amount;

    while(1){
     if(strcmp(merge_sorted[merge_index]->transac_account, master_before[master_index]->acc_num) == 0){

            if (merge_sorted[merge_index]->neg[0] == 'D'){
                    amount = atoi(merge_sorted[merge_index]->others);
                    blc = blc+ amount;
            }
            else{
                amount = atoi(merge_sorted[merge_index]->others);
                blc = blc - amount;

            }
            merge_index+=1;
        }

    else{
        if(blc >= 0){
            master_before[master_index]->acc_neg[0] = '+';
        }
        if(blc < 0){
            master_before[master_index]->acc_neg[0] = '-';
            blc = blc * -1;

        }
        itoa(blc, master_before[master_index]->acc_blc, 10);
        fprintf(fp, "%.20s%.16s%.6s%.1s%015s", master_before[master_index]->acc_name, master_before[master_index]->acc_num, master_before[master_index]->acc_pwd, master_before[master_index]->acc_neg, master_before[master_index]->acc_blc);
        fprintf(fp, "%s", "\n");
        master_index+=1;
        blc = atoi(master_before[master_index]->acc_blc);

        if(master_before[master_index]->acc_neg[0] == '-'){
            blc = blc * -1;

        }

    }
    if(merge_sorted[merge_index] == NULL){
        if(blc >= 0){
            master_before[master_index]->acc_neg[0] = '+';
        }
        if(blc < 0){
            master_before[master_index]->acc_neg[0] = '-';
            blc = blc * -1;

        }
        itoa(blc, master_before[master_index]->acc_blc, 10);
        fprintf(fp, "%.20s%.16s%.6s%.1s%015s", master_before[master_index]->acc_name, master_before[master_index]->acc_num, master_before[master_index]->acc_pwd, master_before[master_index]->acc_neg, master_before[master_index]->acc_blc);
        fprintf(fp, "%s", "\n");
        master_index +=1;
        break;
        }

    }

    while(1){
        if(master_before[master_index] == NULL){
                break;
        }
        fprintf(fp, "%.20s%.16s%.6s%.1s%015s", master_before[master_index]->acc_name, master_before[master_index]->acc_num, master_before[master_index]->acc_pwd, master_before[master_index]->acc_neg, master_before[master_index]->acc_blc);
        master_index += 1;
    }
    fclose(fp);

}
void sort_transaction(char path[], char sort_path[]){
    struct transaction ** transactions = get_transactions(path);
    struct transaction ** transactions_sort = sort_transactions(transactions);
    save_transactions(transactions_sort, sort_path);
}


void write_neg_report(struct master ** upd_master_file, char save_path[]){
    FILE * fp = fopen(save_path, "w");
    int master_index = 0 ;
    while(1){
        if(upd_master_file[master_index] == NULL){
            break;
        }
        if(upd_master_file[master_index]->acc_neg[0] == '-'){
            fprintf(fp, "Name: %.20s Account Number: %.16s Balance: %.1s%.15s",upd_master_file[master_index]->acc_name, upd_master_file[master_index]->acc_num,upd_master_file[master_index]->acc_neg, upd_master_file[master_index]->acc_blc);
            fprintf(fp, "%s", "\n");
            master_index += 1;
        }
        else{
            master_index += 1;
        }

    }
    fclose(fp);


}


int main(){
    sort_transaction("trans711.txt","transac_Sorted711.txt");
    sort_transaction("trans713.txt","transac_Sorted713.txt");

    struct transaction ** trans711 = get_transactions("transac_Sorted711.txt");
    struct transaction ** trans713 = get_transactions("transac_Sorted713.txt");

    merge_transactions(trans711, trans713, "transac_Sorted.txt");
    struct transaction ** merge = get_transactions("transac_Sorted.txt");

    struct master ** master_before = get_master_transactions("master.txt");
    upd_master(merge, master_before, "master_updated.txt");
    struct master ** upd_master_file = get_master_transactions("master_updated.txt");
    write_neg_report(upd_master_file, "negReport.txt");

    return 0;

}

