from collections import Counter
from itertools import repeat
import pandas as pd
import random
import openai
import os
import os.path
import anthropic
import time
from datetime import datetime
import re
import numpy as np
import sys

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

