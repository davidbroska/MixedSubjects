from collections import Counter
from itertools import repeat
import pandas as pd
import numpy as np
import random
import openai
import os
import os.path
import anthropic
import time
import re
import random
import statsmodels.api as sm
from datetime import datetime
from ppi_py import ppi_ols_ci, classical_ols_ci, ppi_ols_pointestimate
from scipy import stats


# ======================================================================
# Function to generate a scenario for the LLM, used in 4_PromptLLM.iypnb
# ======================================================================

def generate_scenario(response, include_persona=True):

    if include_persona:

        # "Annual income, including tips, dividends, interest, etc (in US dollars)" [dropdown menu]
        inc_descriptions = {
            2500:   "under 5,000",                               # "Under $5,000"
            7500:   "7,500",                                     # "$5,000-$10,000"
            12500:  "12,500",                                    # "$10,001-$15,000"
            20000:  "20,000",                                    # "$15,001-$25,000"
            30000:  "30,000",                                    # "$25,001-$35,000" 
            42500:  "42,500",                                    # "$35,001-$50,000"
            65000:  "65,000",                                    # "$50,000-$85,000"
            90000:  "90,000",                                    # "$80,001-$100,000"
            150000: "more than 100,000"                          # "Over $100,000"
        }
        inc_val = response["Review_ContinuousIncome"].unique()[0]
        inc_des = "You earn an annual income of {} US dollars.".format(inc_descriptions[inc_val])


        # "How old are you?" [text box]
        age_val = response["Review_age"].unique()[0]
        age_des = "You are {} years old.".format(age_val)

        # "Highest level of education" [dropdown menu]
        edu_descriptions = {
            'underHigh': "less than a high school diploma",      # "Less than a High School Diploma"
            'high': "a high school diploma",                     # "High School Diploma"
            'vocational': "vocational training",                 # "Vocational training"
            'college': "that you attended college",              # "Attended College"
            'bachelor': "a bachelor degree",                     # "Bachelor Degree"
            'graduate': "graduate degree"                        # "Graduate Degree"
        }
        edu_val = response["Review_education"].unique()[0]
        edu_des = "Your highest level of education is {}.".format(edu_descriptions[edu_val])


        # "What is your gender?" [dropdown menu]
        gen_descriptions = {
            'Man':   "You are a man.",                           # "Male"
            'Woman': "You are a woman.",                         # "Female"
            "Other": "You do not identify as a woman or a man."  # "Other"
        }
        gen_val = response["Review_gender"].unique()[0]
        gen_des = gen_descriptions[gen_val]

        # "What are your religious views?" [slider scale]
        rel_val = response["Review_religious"].unique()[0]
        rel_des = "On a scale from 0 (Not Religious) to 100 (Very Religious), you report a score of {} for your religious views.".format(rel_val)

        # "What are your political views?" [slider scale]
        pol_val = response["Review_political"].unique()[0]
        pol_des = "On a scale from 0 (Conservative) to 100 (Progressive), you report a score of {} for your political views.".format(pol_val)

        # reshuffle order of persona characterists
        persona_characteristics = [inc_des, age_des, edu_des, gen_des, pol_des, rel_des]
        random.shuffle(persona_characteristics)
    
        persona = '\n'.join([f' - {c}' for c in persona_characteristics])
    
    # set seed for order of characters in description of scenarios
    random.seed(a=2024)

    # column names in survey data
    cols = ['Man','Woman','Pregnant','Stroller','OldMan','OldWoman','Boy','Girl','Homeless','LargeWoman','LargeMan','Criminal',
            'MaleExecutive','FemaleExecutive','FemaleAthlete','MaleAthlete','FemaleDoctor','MaleDoctor','Dog','Cat']
    
    # corresponding words used in the descriptions
    character = ["man", "woman", "pregnant woman", "baby", "elderly man", "elderly woman", "boy", "girl", "homeless person", "large woman", "large man",
                 "criminal", "male executive", "female executive", "female athlete", "male athlete", "female doctor", "male doctor", "dog","cat"]

    # plural conversion
    characters_plural = ["men", "women", "pregnant women", "babies", "elderly men", "elderly women", "boys", "girls", "homeless persons", "large women", "large men", 
                         "criminals", "male executives", "female executives", "female athletes", "male athletes", "female doctors", "male doctors", "dogs", "cats"]
    def plural(elem, cnt):
        if cnt > 1:
            return characters_plural[character.index(elem)]
        else:
            return elem
        
    # baseline description
    base_description = "A self-driving car with sudden brake failure will "

    # shortcuts for text
    traffic_light_pattern_descriptions = {
        'red': "flouting the law by crossing on the red signal",
        'green': "abiding by the law by crossing on the green signal" 
    }

    # whether scenarios involve a car and a pedestrian
    is_pasvsped = all(response["PedPed"] == 0)

    # passengers vs pedestrians
    if is_pasvsped:

        pas_description = ""
        ped_description = ""
        
        # passengers died if Barrier=1
        pas = response[ response["Barrier"]==1 ]
        # long format to get characters and frequency
        pas_T = pas[cols].T.reset_index()
        pas_ind = pas_T[pas_T.iloc[:,1] >= 1 ].index
        # get list of unique characters in scenario
        pas_char = [character[i] for i in pas_ind]
        # get number of times each one of them appears
        pas_numchar = list(pas_T[pas_T.iloc[:,1] >= 1 ].iloc[:,1])
        # repeat characters if they occur multiple times
        passengers  = [elem for count, elem in zip(pas_numchar, pas_char) for _ in repeat(None, count)]
        # reshuffle order
        random.shuffle(passengers)

        # pedestrians died if Barrier=0
        ped = response[ response["Barrier"]==0 ]
        # long format to get characters and frequency
        ped_T = ped[cols].T.reset_index()
        ped_ind = ped_T[ped_T.iloc[:,1] >= 1 ].index
        # get list of unique characters in scenario
        ped_char = [character[i] for i in ped_ind]
        # get number of times each one of them appears
        ped_numchar = list(ped_T[ped_T.iloc[:,1] >= 1 ].iloc[:,1])
        # repeat characters if they occur multiple times
        pedestrians = [elem for count, elem in zip(ped_numchar, ped_char) for _ in repeat(None, count)]
        # reshuffle order
        random.shuffle(pedestrians)

        pas_dict = Counter(passengers)
        ped_dict = Counter(pedestrians)

        passengers_set = ""
        nb_passengers_set = 0
        for i, (element, count) in enumerate(pas_dict.items()):
            nb_passengers_set += count
            passengers_set += "{} {}".format(count, plural(element, count))
            if i == len(pas_dict) - 2:
                if len(pas_dict) == 2:
                    passengers_set += " and "
                else:
                    passengers_set += ", and "
            elif i < len(pas_dict) - 1:
                passengers_set += ", "

        pedestrians_set = ""
        nb_pedestrians_set = 0
        for i, (element, count) in enumerate(ped_dict.items()):
            nb_pedestrians_set += count
            pedestrians_set += "{} {}".format(count, plural(element, count))
            if i == len(ped_dict) - 2:
                if len(ped_dict) == 2:
                    pedestrians_set += " and "
                else:
                    pedestrians_set += ", and "
            elif i < len(ped_dict) - 1:
                pedestrians_set += ", "

        # code legality for pedestrians
        if ped.iloc[0]["CrossingSignal"] == 0: 
            ped_signal = "NA" 
        elif ped.iloc[0]["CrossingSignal"] == 1:
            ped_signal = "green"
        elif ped.iloc[0]["CrossingSignal"] == 2:
            ped_signal  = "red"
        
        # passengers died as the result of swerving
        if pas.iloc[0]["Intervention"] == 1:
            pas_description = pas_description + base_description + "swerve and crash into a concrete barrier. This will result in the death of {}, who {} inside the car.".format(
                passengers_set,
                'were' if nb_passengers_set >= 2 else 'was',
            )
            if ped_signal != "NA":
                ped_description = ped_description + base_description + "continue ahead and drive through a pedestrian crossing ahead. This will result in the death of {}, who {} {}.".format(
                    pedestrians_set,
                    'were' if nb_pedestrians_set >= 2 else 'was',
                    traffic_light_pattern_descriptions[ped_signal],
                )
            else:
                ped_description = ped_description + base_description + "continue ahead and drive through a pedestrian crossing ahead. This will result in the death of {}, who {} crossing.".format(
                    pedestrians_set,
                    'were' if nb_pedestrians_set >= 2 else 'was',
                )
        # passengers died from continuing ahead and crashing into a barrier
        else:
            pas_description = pas_description + base_description + "continue ahead and crash into a concrete barrier. This will result in the death of {}, who {} inside the car.".format(
                passengers_set,
                'were' if nb_pedestrians_set >= 2 else 'was',
            )
            if ped_signal != "NA":
                ped_description = ped_description + base_description + "swerve to avoid crashing into a concrete barrier and drive through a pedestrian crossing in the other lane. This will result in the death of {}, who {} {}.".format(
                    pedestrians_set,
                    'were' if nb_pedestrians_set >= 2 else 'was',
                    traffic_light_pattern_descriptions[ped_signal],
                )
            else:
                ped_description = ped_description + base_description + "swerve to avoid crashing into a concrete barrier and drive through a pedestrian crossing in the other lane. This will result in the death of {}, who {} crossing.".format(
                    pedestrians_set,
                    'were' if nb_pedestrians_set >= 2 else 'was',
                )

        pas_description = pas_description + " Meanwhile, it will spare {}".format(pedestrians_set)
        if ped_signal != "NA":
            pas_description = pas_description + ", who {} {}.\n".format(
                'were' if nb_pedestrians_set >= 2 else 'was',
                traffic_light_pattern_descriptions[ped_signal],
            )
        else:
            pas_description = pas_description + ", who {} crossing.\n".format('were' if nb_pedestrians_set >= 2 else 'was')

        ped_description = ped_description + " Meanwhile, it will spare {}".format(passengers_set)
        ped_description = ped_description + ", who {} inside the car.\n".format('were' if nb_passengers_set >= 2 else 'was')

        # order descriptions of scenarios as presented to respondent 
        if pas.iloc[0]["LeftHand"] == 1: 
            c1_description = "Case 1.\n" + pas_description
            c2_description = "Case 2.\n" + ped_description
        else:
            c1_description = "Case 1.\n" + ped_description
            c2_description = "Case 2.\n" + pas_description

    
    # pedestrians vs pedestrians
    else:
            
        ped1_description = ""
        ped2_description = ""
        
        # pedestrians 1 mentioned first if LeftHand=1
        ped1 = response[ response["LeftHand"]==1 ]
        # long format to get characters and frequency
        ped1_T = ped1[cols].T.reset_index()
        ped1_ind = ped1_T[ped1_T.iloc[:,1] >= 1 ].index
        # get list of unique characters in scenario
        ped1_char = [character[i] for i in ped1_ind]
        # get number of times each one of them appears
        ped1_numchar = list(ped1_T[ped1_T.iloc[:,1] >= 1 ].iloc[:,1])
        # repeat characters if they occur multiple times
        pedestrians1 = [elem for count, elem in zip(ped1_numchar, ped1_char) for _ in repeat(None, count)]
        # reshuffle order
        random.shuffle(pedestrians1)

        # pedestrians died if Barrier=0
        ped2 = response[ response["LeftHand"]==0 ]
        # long format to get characters and frequency
        ped2_T = ped2[cols].T.reset_index()
        ped2_ind = ped2_T[ped2_T.iloc[:,1] >= 1 ].index
        # get list of unique characters in scenario
        ped2_char = [character[i] for i in ped2_ind]
        # get number of times each one of them appears
        ped2_numchar = list(ped2_T[ped2_T.iloc[:,1] >= 1 ].iloc[:,1])
        # repeat characters if they occur multiple times
        pedestrians2 = [elem for count, elem in zip(ped2_numchar, ped2_char) for _ in repeat(None, count)]
        # reshuffle order
        random.shuffle(pedestrians2)

        ped1_dict = Counter(pedestrians1)
        ped2_dict = Counter(pedestrians2)

        pedestrians1_set = ""
        nb_pedestrians1_set = 0
        for i, (element, count) in enumerate(ped1_dict.items()):
            nb_pedestrians1_set += count
            pedestrians1_set += "{} {}".format(count, plural(element, count))
            if i == len(ped1_dict) - 2:
                if len(ped1_dict) == 2:
                    pedestrians1_set += " and "
                else:
                    pedestrians1_set += ", and "
            elif i < len(ped1_dict) - 1:
                pedestrians1_set += ", "

        pedestrians2_set = ""
        nb_pedestrians2_set = 0
        for i, (element, count) in enumerate(ped2_dict.items()):
            nb_pedestrians2_set += count
            pedestrians2_set += "{} {}".format(count, plural(element, count))
            if i == len(ped2_dict) - 2:
                if len(ped2_dict) == 2:
                    pedestrians2_set += " and "
                else:
                    pedestrians2_set += ", and "
            elif i < len(ped2_dict) - 1:
                pedestrians2_set += ", "

        # code legality for pedestrians 1
        if ped1.iloc[0]["CrossingSignal"] == 0: 
            ped1_signal = "NA" 
        elif ped1.iloc[0]["CrossingSignal"] == 1:
            ped1_signal = "green"
        elif ped1.iloc[0]["CrossingSignal"] == 2:
            ped1_signal  = "red"

        # code legality for pedestrians 2 
        if ped2.iloc[0]["CrossingSignal"] == 0: 
            ped2_signal = "NA" 
        elif ped2.iloc[0]["CrossingSignal"] == 1:
            ped2_signal = "green"
        elif ped2.iloc[0]["CrossingSignal"] == 2:
            ped2_signal  = "red"
        
        # pedestrians 1 died because respondent let the AV swerve
        if ped1.iloc[0]["Intervention"] == 1:
            if ped1_signal != "NA":
                ped1_description = ped1_description + base_description + "swerve and drive through a pedestrian crossing in the other lane. This will result in the death of {}, who {} {} in the other lane.".format(
                    pedestrians1_set,
                    'were' if nb_pedestrians1_set >= 2 else 'was',
                    traffic_light_pattern_descriptions[ped1_signal],
                )
                ped2_description = ped2_description + base_description + "continue ahead and drive through a pedestrian crossing ahead. This will result in the death of {}, who {} {} ahead of the car.".format(
                    pedestrians2_set,
                    'were' if nb_pedestrians2_set >= 2 else 'was',
                    traffic_light_pattern_descriptions[ped2_signal],
                )
            else:
                ped1_description = ped1_description + base_description + "swerve and drive through a pedestrian crossing in the other lane. This will result in the death of {}, who {} crossing in the other lane.".format(
                    pedestrians1_set,
                    'were' if nb_pedestrians1_set >= 2 else 'was',
                )
                ped2_description = ped2_description + base_description + "continue ahead and drive through a pedestrian crossing ahead. This will result in the death of {}, who {} crossing ahead of the car.".format(
                    pedestrians2_set,
                    'were' if nb_pedestrians2_set >= 2 else 'was',
                )
        # pedestrians 2 died because respondet let the AV stay on course
        else:
            if ped1_signal != "NA":
                ped1_description = ped1_description + base_description + "continue ahead and drive through a pedestrian crossing ahead. This will result in the death of {}, who {} {} ahead of the car.".format(
                    pedestrians1_set,
                    'were' if nb_pedestrians1_set >= 2 else 'was',
                    traffic_light_pattern_descriptions[ped1_signal],
                )
                ped2_description = ped2_description + base_description + "swerve and drive through a pedestrian crossing in the other lane. This will result in the death of {}, who {} {} in the other lane.".format(
                    pedestrians2_set,
                    'were' if nb_pedestrians2_set >= 2 else 'was',
                    traffic_light_pattern_descriptions[ped2_signal],
                )
            else:
                ped1_description = ped1_description + base_description + "continue ahead and drive through a pedestrian crossing ahead. This will result in the death of {}, who {} crossing ahead of the car.".format(
                    pedestrians1_set,
                    'were' if nb_pedestrians1_set >= 2 else 'was',
                )
                ped2_description = ped2_description + base_description + "swerve and drive through a pedestrian crossing in the other lane. This will result in the death of {}, who {} crossing in the other lane.".format(
                    pedestrians2_set,
                    'were' if nb_pedestrians2_set >= 2 else 'was',
                )

        ped1_description = ped1_description + " Meanwhile, it will spare {}".format(pedestrians2_set)
        if ped2_signal != "NA":
            ped1_description = ped1_description + ", who {} {} ".format(
                'were' if nb_pedestrians2_set >= 2 else 'was',
                traffic_light_pattern_descriptions[ped2_signal],
            )
        else:
            ped1_description = ped1_description + ", who {} crossing ".format('were' if nb_pedestrians2_set >= 2 else 'was')

        if ped1.iloc[0]["Intervention"] == 1:
            ped1_description = ped1_description + "ahead of the car.\n"
        else:
            ped1_description = ped1_description + "in the other lane.\n"
        
        ped2_description = ped2_description + " Meanwhile, it will spare {}".format(pedestrians1_set)
        if ped1_signal != "NA":
            ped2_description = ped2_description + ", who {} {} ".format(
                'were' if nb_pedestrians1_set >= 2 else 'was',
                traffic_light_pattern_descriptions[ped1_signal],
            )
        else:
            ped2_description = ped2_description + ", who {} crossing ".format('were' if nb_pedestrians1_set >= 2 else 'was')

        if ped1.iloc[0]["Intervention"] == 1:
            ped2_description = ped2_description + "in the other lane.\n"
        else:
            ped2_description = ped2_description + "ahead of the car.\n"

        c1_description = "Case 1.\n" + ped1_description
        c2_description = "Case 2.\n" + ped2_description

    # system content base description
    system_content = "Carefully consider the following information and think step by step. "

    # add information about survey respondent
    if include_persona:
        system_content += "You are an American with the following characteristics:\n" + persona + "\n"
    else: 
        system_content += "\n"

    # add information about the structure of the scenario      
    if is_pasvsped:
        system_content += "Consider that pedestrians are crossing a zebra crossing that spans two lanes. "
    else:
        system_content += "Consider that pedestrians are crossing a zebra crossing that spans two lanes, divided by an island. "
    
    # add task description and requested output format
    system_content += "Please indicate which case is better for autonomous driving by choosing either \'Case 1\' or \'Case 2\'. Respond with one word only.\n"
    
    # user content contains description of scenario
    user_content = c1_description + "\n" + c2_description

    return system_content, user_content




# ===================================================================================
# Function to classify LLM responses as saved or not saved, used in 4_PromptLLM.iypnb
# ===================================================================================

def classify_response(row, column_name):

    # row refers to one row in the dataframe with llm reponses
    # column_name refers to the column with llm text output
    text = row[ column_name ].lower()

    # define regular expression for whether llm opted for 1 or 2
    pattern_case1 = re.compile(r"case\s?1.?")
    pattern_case2 = re.compile(r"case\s?2.?")

    # whether text matches case1 and case2 pattern, respectively
    match_case1 = pattern_case1.search(text) is not None
    match_case2 = pattern_case2.search(text) is not None

    if match_case1 and not match_case2:
        case = 1
    elif not match_case1 and match_case2:
        case = 2
    else:
        case = np.NaN

    # row refers to case 1 and llm chose case 1, so people described in case 1 are not saved
    if  row["LeftHand"]==1 and case==1: 
        saved = 0
    # row refers to case 1 but llm chose case 2, so people described in case 2 are saved
    elif row["LeftHand"]==1 and case==2:
        saved = 1
    # row refers to case 2 and llm chose case 2, so people described in case 2 are not saved
    elif row["LeftHand"]==0 and case==2:
        saved = 0
    # row refers to case 2 but llm chose case 1, so people described in case 2 are saved
    elif row["LeftHand"]==0 and case==1:
        saved = 1
    else: 
        saved = np.NaN
    

    return saved






# ===========================================================================================
# Function to prompt the LLM with scenarios and save the responses, used in 4_PromptLLM.iypnb
# ===========================================================================================

def prompt_llm(data, model, api_key, csv_path, include_persona=True, verbose=False, sleep=0, temperature=None):

    # data:            DataFrame with scenario descriptions and profiles of survey respondents
    # model:           Name of model, e.g. gpt-4o
    # api_key:         API key 
    # csv_path:        File path to existing .csv for saving output of API calls, creates .csv if not exists
    # include_persona: If true, the prompt includes a demographic description of the survey respondent
    # verbose:         If true, the LLM response is printed
    # sleep:           Time in seconds between API calls

    # prompt 
    if os.path.exists(csv_path): 

        # get existing reponses
        existing = pd.read_csv(csv_path, usecols = ["ResponseID"])
        print("Existing responses in ", csv_path, ":", existing["ResponseID"].unique().shape[0])

        # define column indicating in which dataframe ResponseID is present 
        toprompt = pd.merge(data, existing, indicator=True, on="ResponseID", how="left")

        # keep rows that haven't been used for prompting
        ids_toprompt = toprompt.loc[toprompt['_merge'] == 'left_only', 'ResponseID'].unique()
        random.shuffle(ids_toprompt)

        print("Number of remaining prompts:", len(ids_toprompt))

    else:
        ids_toprompt = data["ResponseID"].unique()
        random.shuffle(ids_toprompt)


    if len(ids_toprompt) > 0: 
        
        i = 1
        for id in ids_toprompt: 

            # track progress
            if i==1 or i % 50 == 0: 
                print(f"Prompt {i} out of {len(ids_toprompt)}")
            i = i+1
            
            survey_response = data[ data["ResponseID"]== id ]

            prompt = generate_scenario(survey_response, include_persona=include_persona)

            
            # check the model and assign the appropriate client
            if model in ["gpt-4o","gpt-4-turbo","o1-mini","o1-preview"]:
                
                client = openai.OpenAI(api_key=api_key)

                messages=[
                        {"role": "system", "content": prompt[0]},
                        {"role": "user",   "content": prompt[1]}
                ]

                if model in ["o1-mini", "o1-preview"]:
                    messages = [{"role": "user",   "content": prompt[0] + " " + prompt[1]}]

                if temperature is None:
                    reply = client.chat.completions.create(
                        model=model, 
                        messages=messages
                    ) 
                elif temperature is not None:
                    reply = client.chat.completions.create(
                        model=model, 
                        messages=messages,
                        temperature=temperature
                    ) 
                
                llm_response = reply.choices[0].message.content


            elif model == "claude-3-5-sonnet-20241022":

                client = anthropic.Anthropic(api_key=api_key)

                reply = client.messages.create(
                    model=model, 
                    max_tokens=4,
                    system=prompt[0],
                    messages=[
                        {"role": "user",   "content": prompt[1]}
                    ],
                )

                llm_response = reply.content[0].text

            else:
                raise ValueError(f"Unsupported model: {model}")
            
            # print LLM response if verbose
            if verbose: print(llm_response)

            # loop with a 1-second pause between iterations
            time.sleep(sleep)

            # prefix based on llm name and whether prompt contained the persona
            column_prefix = re.sub('[-._ ]', '', model) + ("_wp_" if include_persona else "_np_")

            # Create a dictionary for the new row
            new_values = {
                column_prefix+'Timestamp': datetime.now().isoformat(),
                column_prefix+'SystemPrompt': prompt[0],
                column_prefix+'UserPrompt': prompt[1],
                column_prefix+'Persona': int(include_persona),
                column_prefix+'Label': llm_response}        

            survey_response = survey_response.assign(**new_values)

            survey_response[column_prefix+"Saved"] = survey_response.apply(classify_response, column_name=column_prefix+"Label", axis=1)

            if os.path.isfile(csv_path): 
                survey_response.to_csv(csv_path, mode='a', header=False, index=False)
            else: 
                survey_response.to_csv(csv_path, index = False)

    else:
        print("No remaining responses.")





# ======================================================================================
# Function to calculate the weights for estimating the AMCE, used in 7_AnalysisPPI.iypnb
# ======================================================================================

def CalcTheoreticalInt(r):
    # this function is applied to each row (r)
    if r["Intervention"]==0:
        if r["Barrier"]==0:
            if r["PedPed"]==1: p = 0.48
            else: p = 0.32
            
            if r["CrossingSignal"]==0:   p = p * 0.48
            elif r["CrossingSignal"]==1: p = p * 0.2
            else: p = p * 0.32
        else: p = 0.2

    else: 
        if r["Barrier"]==0:
            if r["PedPed"]==1: 
                p = 0.48
                if r["CrossingSignal"]==0: p = p * 0.48
                elif r["CrossingSignal"]==1: p = p * 0.32
                else: p = p * 0.2
            else: 
                p = 0.2
                if r["CrossingSignal"]==0: p = p * 0.48
                elif r["CrossingSignal"]==1: p = p * 0.2
                else: p = p * 0.32
        else: p = 0.32  
    
    return(p)  
        
def calcWeightsTheoretical(profiles):
    
    p = profiles.apply(CalcTheoreticalInt, axis=1)

    weight = 1/p 

    return(weight)  





# =================================================================
# Function to calculate PPI statistics, used in 7_AnalysisPPI.iypnb
# =================================================================

def _ols_get_stats(
    pointest,
    X,
    Y,
    Yhat,
    X_unlabeled,
    Yhat_unlabeled,
    w=None,
    w_unlabeled=None,
    use_unlabeled=True,
):
    """Computes the statistics needed for the OLS-based prediction-powered inference.

    Args:
        pointest (ndarray): A point estimate of the coefficients.
        X (ndarray): Covariates for the labeled data set.
        Y (ndarray): Labels for the labeled data set.
        Yhat (ndarray): Predictions for the labeled data set.
        X_unlabeled (ndarray): Covariates for the unlabeled data set.
        Yhat_unlabeled (ndarray): Predictions for the unlabeled data set.
        w (ndarray, optional): Sample weights for the labeled data set.
        w_unlabeled (ndarray, optional): Sample weights for the unlabeled data set.
        use_unlabeled (bool, optional): Whether to use the unlabeled data set.

    Returns:
        grads (ndarray): Gradient of the loss function with respect to the coefficients.
        grads_hat (ndarray): Gradient of the loss function with respect to the coefficients, evaluated using the labeled predictions.
        grads_hat_unlabeled (ndarray): Gradient of the loss function with respect to the coefficients, evaluated using the unlabeled predictions.
        inv_hessian (ndarray): Inverse Hessian of the loss function with respect to the coefficients.
    """
    n = Y.shape[0]
    N = Yhat_unlabeled.shape[0]
    d = X.shape[1]
    w = np.ones(n) if w is None else w / np.sum(w) * n
    w_unlabeled = (
        np.ones(N)
        if w_unlabeled is None
        else w_unlabeled / np.sum(w_unlabeled) * N
    )

    hessian = np.zeros((d, d))
    grads_hat_unlabeled = np.zeros(X_unlabeled.shape)
    if use_unlabeled:
        for i in range(N):
            hessian += (
                w_unlabeled[i]
                / (N + n)
                * np.outer(X_unlabeled[i], X_unlabeled[i])
            )
            grads_hat_unlabeled[i, :] = (
                w_unlabeled[i]
                * X_unlabeled[i, :]
                * (np.dot(X_unlabeled[i, :], pointest) - Yhat_unlabeled[i])
            )

    grads = np.zeros(X.shape)
    grads_hat = np.zeros(X.shape)
    for i in range(n):
        hessian += (
            w[i] / (N + n) * np.outer(X[i], X[i])
            if use_unlabeled
            else w[i] / n * np.outer(X[i], X[i])
        )
        grads[i, :] = w[i] * X[i, :] * (np.dot(X[i, :], pointest) - Y[i])
        grads_hat[i, :] = (
            w[i] * X[i, :] * (np.dot(X[i, :], pointest) - Yhat[i])
        )

    inv_hessian = np.linalg.inv(hessian).reshape(d, d)
    return grads, grads_hat, grads_hat_unlabeled, inv_hessian

def _power_analysis_stats(grads, grads_hat, inv_hessian):
    grads_ = grads - grads.mean(axis=0)
    grads_hat_ = grads_hat - grads_hat.mean(axis=0)
    cov = inv_hessian @ (grads_[:,None,:] * grads_hat_[:,:,None]).mean(axis=0) @ inv_hessian
    var = inv_hessian @ (grads_[:,None,:]*grads_[:,:,None]).mean(axis=0) @ inv_hessian
    var_hat = inv_hessian @ (grads_hat_[:,None,:]*grads_hat_[:,:,None]).mean(axis=0) @ inv_hessian
    rhos_sq = np.diag(cov)**2/(np.diag(var)*np.diag(var_hat))
    sigmas_sq = np.diag(var)
    return rhos_sq, sigmas_sq

def _estimate_ppi_SE(n, N, rho_sq, var_Y):
    if N == np.inf:
        return np.sqrt(var_Y*(1-rho_sq)/n)
    if N == 0:
        return np.sqrt(var_Y/n)
    var_ppi = var_Y*(1-rho_sq*N/(n+N))/n
    return np.sqrt(var_ppi)

def _estimate_classical_SE(n, var_Y):
    return np.sqrt(var_Y/n)






# =========================================================
# Function to compute the AMCE, used in 7_AnalysisPPI.iypnb
# =========================================================

def compute_amce(data, x, y, alpha=0.05):

    # specify regression for swerve or stay in lane
    if x=="Intervention":
        
        # calculate weights
        data.loc[:,"weights"] = calcWeightsTheoretical(data)
    
        # drop rows with missing values on dependent variable
        dd = data.dropna(subset=y)

        # if X=1 characters die if AV serves, if X=0 characters if AV stays
        X = dd["Intervention"]
        X = sm.add_constant(X)

        # define model with standard errors clustered on UserID
        model = sm.WLS(dd[y], X, weights=dd["weights"])
    

    # specify regression for relationship to vehicle
    if x=="Barrier":

        # consider only dilemmas without legality and only pedestrians vs passengers
        data_sub = data.loc[(data["CrossingSignal"]==0) & (data["PedPed"]==0), :].copy()

        # calculate weights
        data_sub.loc[:,"weights"] = calcWeightsTheoretical(data_sub)

        # drop rows with missing values on dependent variable
        dd = data_sub.dropna(subset=y)
        
        # if X=1 passengers die and if X=0 pedestrians die
        X = dd["Barrier"]

        # recode to estimate the preference for pedestrians over passengers 
        X = 1 - X
        X = sm.add_constant(X)

        # define model with standard errors clustered on UserID
        model = sm.WLS(dd[y], X, weights=dd["weights"])

    
    # specify regression for legality
    if x=="CrossingSignal": 
        
        # consider dilemmas with legality and only pedestrians vs pedestrians
        data_sub = data.loc[(data["CrossingSignal"]!=0) & (data["PedPed"]==1), :].copy()

        # calculate weights
        data_sub.loc[:,"weights"] = calcWeightsTheoretical(data_sub)

        # drop rows with missing values on dependent variable
        dd = data_sub.dropna(subset=y)

        # if X=1 pedestrians cross on a green light, if X=2 pedestrians cross on a red light 
        X = dd["CrossingSignal"]

        # create dummy variable to estimate preference for pedestrians that cross legally (1) vs legally (0)
        X = 2 - X 
        X = sm.add_constant(X)

        # define model with standard errors clustered on UserID
        model = sm.WLS(dd[y], X, weights=dd["weights"])

    

    # Specify regressions for the remaining six attributes
    if x=="Utilitarian":
        
        # consider dilemmas that compare 'More' versus 'Less' characters
        data_sub = data.loc[(data["ScenarioType"]=="Utilitarian") & (data["ScenarioTypeStrict"]=="Utilitarian"), :].copy()

        # calculate weights
        data_sub.loc[:,"weights"] = calcWeightsTheoretical(data_sub)

        # drop rows with missing values on dependent variable
        dd = data_sub.dropna(subset=y)
        dd = dd.rename(columns = {'AttributeLevel': 'Utilitarian'})

        # create dummy variable to estimate the preference for sparing more characters
        X = (dd.loc[:,"Utilitarian"]=="More").astype(int)
        X = sm.add_constant(X)

        # define model with standard errors clustered on UserID
        model = sm.WLS(dd[y], X, weights=dd["weights"])


    if x=="Species":
        
        # consider dilemmas that compare humans versus animals 
        data_sub = data.loc[(data["ScenarioType"]=="Species") & (data["ScenarioTypeStrict"]=="Species"), :].copy()

        # calculate weights
        data_sub.loc[:,"weights"] = calcWeightsTheoretical(data_sub)

        # drop rows with missing values on dependent variable
        dd = data_sub.dropna(subset=y)
        dd = dd.rename(columns = {'AttributeLevel': 'Species'})

        # create dummy variable to estimate the preference for sparing humans
        X = (dd.loc[:,"Species"]=="Hoomans").astype(int)
        X = sm.add_constant(X)

        # define model with standard errors clustered on UserID
        model = sm.WLS(dd[y], X, weights=dd["weights"])
    

    if x=="Gender":
        
        # consider dilemmas that compare women versus men
        data_sub = data.loc[(data["ScenarioType"]=="Gender") & (data["ScenarioTypeStrict"]=="Gender"), :].copy()

        # calculate weights
        data_sub.loc[:,"weights"] = calcWeightsTheoretical(data_sub)

        # drop rows with missing values on dependent variable
        dd = data_sub.dropna(subset=y)
        dd = dd.rename(columns = {'AttributeLevel': 'Gender'})

        # create dummy variable to estimate the preference for sparing women
        X = (dd.loc[:,"Gender"]=="Female").astype(int)
        X = sm.add_constant(X)

        # define model with standard errors clustered on UserID
        model = sm.WLS(dd[y], X, weights=dd["weights"])


    if x=="Fitness":
        
        # consider dilemmas that compare fit characters versus those that are not
        data_sub = data.loc[(data["ScenarioType"]=="Fitness") & (data["ScenarioTypeStrict"]=="Fitness"), :].copy()

        # calculate weights
        data_sub.loc[:,"weights"] = calcWeightsTheoretical(data_sub)

        # drop rows with missing values on dependent variable
        dd = data_sub.dropna(subset=y)
        dd = dd.rename(columns = {'AttributeLevel': 'Fitness'})

        # create dummy variable to estimate the preference for sparing fit characters
        X = (dd.loc[:,"Fitness"]=="Fit").astype(int)
        X = sm.add_constant(X)

        # define model with standard errors clustered on UserID
        model = sm.WLS(dd[y], X, weights=dd["weights"])


    if x=="Age":
        
        # consider dilemmas that compare younger versus older characters
        data_sub = data.loc[(data["ScenarioType"]=="Age") & (data["ScenarioTypeStrict"]=="Age"), :].copy()

        # calculate weights
        data_sub.loc[:,"weights"] = calcWeightsTheoretical(data_sub)

        # drop rows with missing values on dependent variable
        dd = data_sub.dropna(subset=y)
        dd = dd.rename(columns = {'AttributeLevel': 'Age'})

        # create dummy variable to estimate the preference for sparing younger characters
        X = (dd.loc[:,"Age"]=="Young").astype(int)
        X = sm.add_constant(X)

        # define model with standard errors clustered on UserID
        model = sm.WLS(dd[y], X, weights=dd["weights"])

    
    if x=="Social Status":
        
        # consider dilemmas that compare high status versus low status characters
        data_sub = data.loc[(data["ScenarioType"]=="Social Status") & (data["ScenarioTypeStrict"]=="Social Status"), :].copy()

        # calculate weights
        data_sub.loc[:,"weights"] = calcWeightsTheoretical(data_sub)

        # drop rows with missing values on dependent variable
        dd = data_sub.dropna(subset=y)
        dd = dd.rename(columns = {'AttributeLevel': 'Social Status'})

        # create dummy variable to estimate the preference for sparing high status characters
        X = (dd.loc[:,"Social Status"]=="High").astype(int)
        X = sm.add_constant(X)

        # define model with standard errors clustered on UserID
        model = sm.WLS(dd[y], X, weights=dd["weights"])



    # fit model and extract estimates
    fit = model.fit(cov_type = 'cluster', cov_kwds = {'groups': dd["UserID"]})
    coef = fit.params[x]
    se = fit.bse[x]
    ci = fit.conf_int(alpha=alpha).loc[x]

    # store results
    res = pd.DataFrame({
        'x': [x],
        'y': [y],
        'beta': [coef],
        'se': [se],
        'lower': [ci[0]],
        'upper': [ci[1]]
    })

    return(res)




# ==================================================================
# Function to compute the AMCE with PPI, used in 7_AnalysisPPI.iypnb
# ==================================================================

def compute_amce_ppi(n_data, N_data, x, y, alpha=0.05):

    # specify regression for swerve or stay in lane
    if x=="Intervention":
        
        # calculate weights
        n_data.loc[:,"weights"] = calcWeightsTheoretical(n_data)
        N_data.loc[:,"weights"] = calcWeightsTheoretical(N_data)
    
        # drop rows with missing values on dependent variable
        n_dd = n_data.dropna(subset=y)
        N_dd = N_data.dropna(subset=y)

        # if X=1 characters die if AV serves, if X=0 characters if AV stays
        n_X = n_dd["Intervention"]               
        N_X = N_dd["Intervention"]

        # add intercept
        n_X = np.column_stack((np.ones(n_X.shape[0]), n_X))
        N_X = np.column_stack((np.ones(N_X.shape[0]), N_X))

        # gold standard data
        n_Y_human   = n_dd["Saved"].to_numpy()    # observed outcomes
        n_Y_silicon = n_dd[y].to_numpy()          # predicted outcomes
        n_weights = n_dd["weights"].to_numpy()    # define weights

        # unlabeled data
        N_Y_silicon = N_dd[y].to_numpy()          # predicted outcomes
        N_weights = N_dd["weights"].to_numpy()    # define weights



    # specify regression for relationship to vehicle
    if x=="Barrier":

        # consider only dilemmas without legality and only pedestrians vs passengers
        n_data_sub = n_data.loc[(n_data["CrossingSignal"]==0) & (n_data["PedPed"]==0), :].copy()
        N_data_sub = N_data.loc[(N_data["CrossingSignal"]==0) & (N_data["PedPed"]==0), :].copy()

        # calculate weights
        n_data_sub.loc[:,"weights"] = calcWeightsTheoretical(n_data_sub)
        N_data_sub.loc[:,"weights"] = calcWeightsTheoretical(N_data_sub)

        # drop rows with missing values on dependent variable
        n_dd = n_data_sub.dropna(subset=y)
        N_dd = N_data_sub.dropna(subset=y)
        
        # if X=1 passengers die and if X=0 pedestrians die
        n_X = n_dd["Barrier"]
        N_X = N_dd["Barrier"]

        # recode to estimate the preference for pedestrians over passengers 
        n_X = 1 - n_X
        N_X = 1 - N_X

        # add intercept
        n_X = np.column_stack((np.ones(n_X.shape[0]), n_X))
        N_X = np.column_stack((np.ones(N_X.shape[0]), N_X))

        # gold standard data
        n_Y_human   = n_dd["Saved"].to_numpy()    # observed outcomes
        n_Y_silicon = n_dd[y].to_numpy()          # predicted outcomes
        n_weights = n_dd["weights"].to_numpy()    # define weights

        # unlabeled data
        N_Y_silicon = N_dd[y].to_numpy()          # predicted outcomes
        N_weights = N_dd["weights"].to_numpy()    # define weights

    

    # specify regression for legality
    if x=="CrossingSignal": 
        
        # consider dilemmas with legality and only pedestrians vs pedestrians
        n_data_sub = n_data.loc[(n_data["CrossingSignal"]!=0) & (n_data["PedPed"]==1), :].copy()
        N_data_sub = N_data.loc[(N_data["CrossingSignal"]!=0) & (N_data["PedPed"]==1), :].copy()

        # calculate weights
        n_data_sub.loc[:,"weights"] = calcWeightsTheoretical(n_data_sub)
        N_data_sub.loc[:,"weights"] = calcWeightsTheoretical(N_data_sub)

        # drop rows with missing values on dependent variable
        n_dd = n_data_sub.dropna(subset=y)
        N_dd = N_data_sub.dropna(subset=y)

        # if X=1 pedestrians cross on a green light, if X=2 pedestrians cross on a red light 
        n_X = n_dd["CrossingSignal"]
        N_X = N_dd["CrossingSignal"]

        # create dummy variable to estimate preference for pedestrians that cross legally (1) vs legally (0)
        n_X = 2 - n_X 
        N_X = 2 - N_X 

        # add intercept
        n_X = np.column_stack((np.ones(n_X.shape[0]), n_X))
        N_X = np.column_stack((np.ones(N_X.shape[0]), N_X))

        # gold standard data
        n_Y_human   = n_dd["Saved"].to_numpy()    # observed outcomes
        n_Y_silicon = n_dd[y].to_numpy()          # predicted outcomes
        n_weights = n_dd["weights"].to_numpy()    # define weights

        # unlabeled data
        N_Y_silicon = N_dd[y].to_numpy()          # predicted outcomes
        N_weights = N_dd["weights"].to_numpy()    # define weights
    


    # Specify regressions for the remaining six attributes
    if x=="Utilitarian":
        
        # consider dilemmas that compare 'More' versus 'Less' characters
        n_data_sub = n_data.loc[(n_data["ScenarioType"]=="Utilitarian") & (n_data["ScenarioTypeStrict"]=="Utilitarian"), :].copy()
        N_data_sub = N_data.loc[(N_data["ScenarioType"]=="Utilitarian") & (N_data["ScenarioTypeStrict"]=="Utilitarian"), :].copy()

        # calculate weights
        n_data_sub.loc[:,"weights"] = calcWeightsTheoretical(n_data_sub)
        N_data_sub.loc[:,"weights"] = calcWeightsTheoretical(N_data_sub)

        # drop rows with missing values on dependent variable
        n_dd = n_data_sub.dropna(subset=y)
        N_dd = N_data_sub.dropna(subset=y)
        
        # rename column to extract coefficient from result
        n_dd = n_dd.rename(columns = {'AttributeLevel': 'Utilitarian'})
        N_dd = N_dd.rename(columns = {'AttributeLevel': 'Utilitarian'})

        # create dummy variable to estimate the preference for sparing more characters
        n_X = (n_dd.loc[:,"Utilitarian"]=="More").astype(int)
        N_X = (N_dd.loc[:,"Utilitarian"]=="More").astype(int)

        # add intercept
        n_X = np.column_stack((np.ones(n_X.shape[0]), n_X))
        N_X = np.column_stack((np.ones(N_X.shape[0]), N_X))

        # gold standard data
        n_Y_human   = n_dd["Saved"].to_numpy()    # observed outcomes
        n_Y_silicon = n_dd[y].to_numpy()          # predicted outcomes
        n_weights = n_dd["weights"].to_numpy()    # define weights

        # unlabeled data
        N_Y_silicon = N_dd[y].to_numpy()          # predicted outcomes
        N_weights = N_dd["weights"].to_numpy()    # define weights



    if x=="Species":
        
        # consider dilemmas that compare humans versus animals 
        n_data_sub = n_data.loc[(n_data["ScenarioType"]=="Species") & (n_data["ScenarioTypeStrict"]=="Species"), :].copy()
        N_data_sub = N_data.loc[(N_data["ScenarioType"]=="Species") & (N_data["ScenarioTypeStrict"]=="Species"), :].copy()

        # calculate weights
        n_data_sub.loc[:,"weights"] = calcWeightsTheoretical(n_data_sub)
        N_data_sub.loc[:,"weights"] = calcWeightsTheoretical(N_data_sub)

        # drop rows with missing values on dependent variable
        n_dd = n_data_sub.dropna(subset=y)
        N_dd = N_data_sub.dropna(subset=y)

        # rename column to extract coefficient from result
        n_dd = n_dd.rename(columns = {'AttributeLevel': 'Species'})
        N_dd = N_dd.rename(columns = {'AttributeLevel': 'Species'})

        # create dummy variable to estimate the preference for sparing humans
        n_X = (n_dd.loc[:,"Species"]=="Hoomans").astype(int)
        N_X = (N_dd.loc[:,"Species"]=="Hoomans").astype(int)

        # add intercept
        n_X = np.column_stack((np.ones(n_X.shape[0]), n_X))
        N_X = np.column_stack((np.ones(N_X.shape[0]), N_X))

        # gold standard data
        n_Y_human   = n_dd["Saved"].to_numpy()    # observed outcomes
        n_Y_silicon = n_dd[y].to_numpy()          # predicted outcomes
        n_weights = n_dd["weights"].to_numpy()    # define weights

        # unlabeled data
        N_Y_silicon = N_dd[y].to_numpy()          # predicted outcomes
        N_weights = N_dd["weights"].to_numpy()    # define weights

    

    if x=="Gender":
        
        # consider dilemmas that compare women versus men
        n_data_sub = n_data.loc[(n_data["ScenarioType"]=="Gender") & (n_data["ScenarioTypeStrict"]=="Gender"), :].copy()
        N_data_sub = N_data.loc[(N_data["ScenarioType"]=="Gender") & (N_data["ScenarioTypeStrict"]=="Gender"), :].copy()

        # calculate weights
        n_data_sub.loc[:,"weights"] = calcWeightsTheoretical(n_data_sub)
        N_data_sub.loc[:,"weights"] = calcWeightsTheoretical(N_data_sub)

        # drop rows with missing values on dependent variable
        n_dd = n_data_sub.dropna(subset=y)
        N_dd = N_data_sub.dropna(subset=y)

        # rename column to extract coefficient from result
        n_dd = n_dd.rename(columns = {'AttributeLevel': 'Gender'})
        N_dd = N_dd.rename(columns = {'AttributeLevel': 'Gender'})

        # create dummy variable to estimate the preference for sparing women
        n_X = (n_dd.loc[:,"Gender"]=="Female").astype(int)
        N_X = (N_dd.loc[:,"Gender"]=="Female").astype(int)

        # add intercept
        n_X = np.column_stack((np.ones(n_X.shape[0]), n_X))
        N_X = np.column_stack((np.ones(N_X.shape[0]), N_X))

        # gold standard data
        n_Y_human   = n_dd["Saved"].to_numpy()    # observed outcomes
        n_Y_silicon = n_dd[y].to_numpy()          # predicted outcomes
        n_weights = n_dd["weights"].to_numpy()    # define weights

        # unlabeled data
        N_Y_silicon = N_dd[y].to_numpy()          # predicted outcomes
        N_weights = N_dd["weights"].to_numpy()    # define weights



    if x=="Fitness":
        
        # consider dilemmas that compare fit characters versus those that are not
        n_data_sub = n_data.loc[(n_data["ScenarioType"]=="Fitness") & (n_data["ScenarioTypeStrict"]=="Fitness"), :].copy()
        N_data_sub = N_data.loc[(N_data["ScenarioType"]=="Fitness") & (N_data["ScenarioTypeStrict"]=="Fitness"), :].copy()

        # calculate weights
        n_data_sub.loc[:,"weights"] = calcWeightsTheoretical(n_data_sub)
        N_data_sub.loc[:,"weights"] = calcWeightsTheoretical(N_data_sub)

        # drop rows with missing values on dependent variable
        n_dd = n_data_sub.dropna(subset=y)
        N_dd = N_data_sub.dropna(subset=y)

        # rename column to extract coefficient from result
        n_dd = n_dd.rename(columns = {'AttributeLevel': 'Fitness'})
        N_dd = N_dd.rename(columns = {'AttributeLevel': 'Fitness'})

        # create dummy variable to estimate the preference for sparing fit characters
        n_X = (n_dd.loc[:,"Fitness"]=="Fit").astype(int)
        N_X = (N_dd.loc[:,"Fitness"]=="Fit").astype(int)

        # add intercept
        n_X = np.column_stack((np.ones(n_X.shape[0]), n_X))
        N_X = np.column_stack((np.ones(N_X.shape[0]), N_X))

        # gold standard data
        n_Y_human   = n_dd["Saved"].to_numpy()    # observed outcomes
        n_Y_silicon = n_dd[y].to_numpy()          # predicted outcomes
        n_weights = n_dd["weights"].to_numpy()    # define weights

        # unlabeled data
        N_Y_silicon = N_dd[y].to_numpy()          # predicted outcomes
        N_weights = N_dd["weights"].to_numpy()    # define weights



    if x=="Age":
        
        # consider dilemmas that compare younger versus older characters
        n_data_sub = n_data.loc[(n_data["ScenarioType"]=="Age") & (n_data["ScenarioTypeStrict"]=="Age"), :].copy()
        N_data_sub = N_data.loc[(N_data["ScenarioType"]=="Age") & (N_data["ScenarioTypeStrict"]=="Age"), :].copy()

        # calculate weights
        n_data_sub.loc[:,"weights"] = calcWeightsTheoretical(n_data_sub)
        N_data_sub.loc[:,"weights"] = calcWeightsTheoretical(N_data_sub)

        # drop rows with missing values on dependent variable
        n_dd = n_data_sub.dropna(subset=y)
        N_dd = N_data_sub.dropna(subset=y)

        # rename column to extract coefficient from result
        n_dd = n_dd.rename(columns = {'AttributeLevel': 'Age'})
        N_dd = N_dd.rename(columns = {'AttributeLevel': 'Age'})

        # create dummy variable to estimate the preference for sparing younger characters
        n_X = (n_dd.loc[:,"Age"]=="Young").astype(int)
        N_X = (N_dd.loc[:,"Age"]=="Young").astype(int)

        # add intercept
        n_X = np.column_stack((np.ones(n_X.shape[0]), n_X))
        N_X = np.column_stack((np.ones(N_X.shape[0]), N_X))

        # gold standard data
        n_Y_human   = n_dd["Saved"].to_numpy()    # observed outcomes
        n_Y_silicon = n_dd[y].to_numpy()          # predicted outcomes
        n_weights = n_dd["weights"].to_numpy()    # define weights

        # unlabeled data
        N_Y_silicon = N_dd[y].to_numpy()          # predicted outcomes
        N_weights = N_dd["weights"].to_numpy()    # define weights


    
    if x=="Social Status":
        
        # consider dilemmas that compare high status versus low status characters
        n_data_sub = n_data.loc[(n_data["ScenarioType"]=="Social Status") & (n_data["ScenarioTypeStrict"]=="Social Status"), :].copy()
        N_data_sub = N_data.loc[(N_data["ScenarioType"]=="Social Status") & (N_data["ScenarioTypeStrict"]=="Social Status"), :].copy()

        # calculate weights
        n_data_sub.loc[:,"weights"] = calcWeightsTheoretical(n_data_sub)
        N_data_sub.loc[:,"weights"] = calcWeightsTheoretical(N_data_sub)

        # drop rows with missing values on dependent variable
        n_dd = n_data_sub.dropna(subset=y)
        N_dd = N_data_sub.dropna(subset=y)

        # rename column to extract coefficient from result
        n_dd = n_dd.rename(columns = {'AttributeLevel': 'Social Status'})
        N_dd = N_dd.rename(columns = {'AttributeLevel': 'Social Status'})

        # create dummy variable to estimate the preference for sparing high status characters
        n_X = (n_dd.loc[:,"Social Status"]=="High").astype(int)
        N_X = (N_dd.loc[:,"Social Status"]=="High").astype(int)

        # add intercept
        n_X = np.column_stack((np.ones(n_X.shape[0]), n_X))
        N_X = np.column_stack((np.ones(N_X.shape[0]), N_X))

        # gold standard data
        n_Y_human   = n_dd.loc[:,"Saved"].to_numpy()    # observed outcomes
        n_Y_silicon = n_dd.loc[:,y].to_numpy()          # predicted outcomes
        n_weights = n_dd.loc[:,"weights"].to_numpy()    # define weights

        # unlabeled data
        N_Y_silicon = N_dd[y].to_numpy()                # predicted outcomes
        N_weights = N_dd.loc[:,"weights"].to_numpy()    # define weights


    # calculate point estimate
    beta_ppi = ppi_ols_pointestimate(X=n_X, Y=n_Y_human, Yhat=n_Y_silicon, 
                                     X_unlabeled=N_X, Yhat_unlabeled=N_Y_silicon, 
                                     w=n_weights, w_unlabeled=N_weights,
                                     coord=1)
    
    # using ppi function to calculate point estimates (lambda=0)
    beta_hum = ppi_ols_pointestimate(X=n_X, Y=n_Y_human, Yhat=n_Y_silicon, 
                                     X_unlabeled=N_X, Yhat_unlabeled=N_Y_silicon, 
                                     w=n_weights, w_unlabeled=N_weights, 
                                     lam=0)
    
    beta_sil = ppi_ols_pointestimate(X=N_X, Y=N_Y_silicon, Yhat=N_Y_silicon, 
                                     X_unlabeled=N_X, Yhat_unlabeled=N_Y_silicon, 
                                     w=N_weights, w_unlabeled=N_weights, 
                                     lam=0)
    
    # using statsmodels to calculate point estimates (same results as with PPI)
    beta_hum_sm = sm.WLS(endog=n_Y_human, exog=n_X, weights=n_weights).fit().params[1]
    beta_sil_sm = sm.WLS(endog=N_Y_silicon, exog=N_X, weights=N_weights).fit().params[1]

    # calculate confidence intervals for PPI, human subjects, and silicon subjects
    lower_CI_ppi, upper_CI_ppi = ppi_ols_ci(X=n_X, Y=n_Y_human, Yhat=n_Y_silicon, 
                                            X_unlabeled=N_X, Yhat_unlabeled=N_Y_silicon, 
                                            w=n_weights, w_unlabeled=N_weights, alpha=alpha,
                                            coord=1)
    
    lower_CI_hum, upper_CI_hum = classical_ols_ci(X=n_X, Y=n_Y_human, w=n_weights, alpha=alpha)

    lower_CI_sil, upper_CI_sil = classical_ols_ci(X=N_X, Y=N_Y_silicon, w=N_weights, alpha=alpha)


    # zscore for two tailed test
    z = stats.norm.ppf(0.975)
    
    # calculate standard errors for PPI, human subjects, and silicon subjects
    se_ppi = (upper_CI_ppi[1] - lower_CI_ppi[1]) / (2 * z)
    
    se_hum = (upper_CI_hum[1] - lower_CI_hum[1]) / (2 * z)

    se_sil = (upper_CI_sil[1] - lower_CI_sil[1]) / (2 * z)
    

    # calculate rho
    beta = sm.WLS(n_Y_human, n_X, weights=n_weights).fit().params

    grads, grads_hat, grads_hat_unlabeled, inv_hessian = _ols_get_stats(
        pointest=beta, 
        X=n_X,
        Y=n_Y_human,
        Yhat= n_Y_silicon,
        X_unlabeled=N_X,
        Yhat_unlabeled=N_Y_silicon,
        w=n_weights,
        w_unlabeled=N_weights,
        use_unlabeled=False)
    
    rho_sq, var_y = _power_analysis_stats(grads, grads_hat, inv_hessian)

    # create and return the output DataFrame
    output_df = pd.DataFrame({
        "y": y,                              
        "x": x,                               # Predictor variable (scenario attribute)
        "beta_ppi": beta_ppi[1],              # PPI point estimate
        "beta_hum": beta_hum[1],              # Human subjects point estimate
        #"beta_hum_sm": beta_hum_sm,           # Human subjects point estimate (statsmodels)
        "beta_sil": beta_sil[1],              # Silicon subjects point estimate
        "beta_sil_sm": beta_sil_sm,           # Silicon subjects point estimate (statsmodels)
        "se_ppi": se_ppi,                     # PPI standard error
        "se_hum": se_hum,                     # Human subjects standard error
        "se_sil": se_sil,                     # Silicon subjects standard error
        "lower_ppi": lower_CI_ppi[1],         # The lower bound of the PPI confidence interval
        "upper_ppi": upper_CI_ppi[1],         # The upper bound of the PPI confidence interval
        "lower_hum": lower_CI_hum[1],         # The lower bound of the human subjects confidence interval
        "upper_hum": upper_CI_hum[1],         # The upper bound of the human subjects confidence interval
        "lower_sil": lower_CI_sil[1],         # The lower bound of the silicon subjects confidence interval
        "upper_sil": upper_CI_sil[1],         # The upper bound of the silicon subjects confidence interval
        "ppi_corr": np.sqrt(rho_sq[1])},      # The association between predictions and outcomes
        index=[0])
    
    return output_df 



def loop_attribute(x):     
    
    print(f"Scenario attribute: {x}")

    df = pd.read_csv("../Data/4_gpt4turbo_wp_20241118.csv.gz")

    # sample size of human subjects
    ns = [10000]

    # multiples of human subjects sample size
    ks = list([0.1, 0.25, 0.5, 0.75]) + list(np.arange(1, 10.5, 0.5))
    
    # number of repetitions for combinations of n and N
    reps = 500

    # LLM predictions
    y= "gpt4turbo_wp_Saved"
    
    result = pd.DataFrame()
    
    # Loop over human subjects sample sizes
    for n in ns:
        
        # N as multiple of n
        Ns = [int(n * k) for k in ks]
        
        # Loop over silicon subjects sample sizes
        for N in Ns:

            print(f"    Silicon sample size: {N}")
            
            # Loop over repetitions
            for r in range(reps):

                # Sample dilemmas for human subjects sample
                df_human = df.sample(n=n, replace=False)

                # Get remaining dilemma ids to sample from
                remaining_df = df.drop(df_human.index)

                # Sample dilemmas for silicon subjects sample
                df_silicon = remaining_df.sample(n=N, replace=False)

                # Compute acme on n human subjects and N silicon subjects
                ppi = compute_amce_ppi(n_data=df_human, N_data=df_silicon, x=x, y=y)

                # Store sample sizes 
                ppi["n"] = n
                ppi["N"] = N

                result = pd.concat([result, ppi], ignore_index=True) 

    # Store number of repetitions to produce results
    result["reps"] = reps

    return result