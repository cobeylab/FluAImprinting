########################
#
#      DAHVcohort subtype
#
########################

DAHVcohort_subtype = function(pars){
    if (profile){
        for (parameter in names(fixed_pars)){
            pars[parameter] = fixed_pars[[parameter]]
        }
    }

    H3m = pars['H3m']
    H1m = pars['H1m']


    h1_imprinting_prob = m_h1
    h2_imprinting_prob = m_h2
    h3_imprinting_prob = m_h3

    # Set imprinting_contribution
    imprinting_contribution_h1 = (1 - H1m * h1_imprinting_prob)
    imprinting_contribution_h3 = (1 - H3m * h3_imprinting_prob)
    
    # Vaccine effectiveness
    ve_h1 = get_vac_contribution_cohort(pars, p0, 'h1')
    ve_h3 = get_vac_contribution_cohort(pars, p0, 'h3')

    # Calculate age contribution
    age_contribution = get_age_contribution(pars, p0)
    
    baseline_h1 = p0 * imprinting_contribution_h1 * age_contribution * nursing_home_data
    baseline_h3 = p0 * imprinting_contribution_h3 * age_contribution * nursing_home_data
    
    ppH1_unvac = baseline_h1 * age_specific_unvac  * (1 - vacc_coverage_h1)
    ppH1_vac = baseline_h1 * age_specific_vac * vacc_coverage_h1 * (1 - ve_h1)
    
    ppH3_unvac = baseline_h3 * age_specific_unvac  * (1 - vacc_coverage_h3)
    ppH3_vac = baseline_h3 * age_specific_vac * vacc_coverage_h3 * (1 - ve_h3)
    
    h1_norm = rowSums(ppH1_unvac, na.rm=TRUE) + rowSums(ppH1_vac, na.rm=TRUE)
    h3_norm = rowSums(ppH3_unvac, na.rm=TRUE) + rowSums(ppH3_vac, na.rm=TRUE)
    
    ppH1_unvac = ppH1_unvac / h1_norm
    ppH1_vac = ppH1_vac / h1_norm
    
    ppH3_unvac = ppH3_unvac / h3_norm
    ppH3_vac = ppH3_vac / h3_norm
    
    pp = list(ppH1_unvac = ppH1_unvac,
              ppH1_vac = ppH1_vac,
              ppH3_unvac = ppH3_unvac,
              ppH3_vac = ppH3_vac)

    return(pp)
}


########################
#
#      DAHNVcohort group
#
########################

DAHNVcohort_group = function(pars){
    if (profile){
        for (parameter in names(fixed_pars)){
            pars[parameter] = fixed_pars[[parameter]]
        }
    }

    H3m = pars['H3m']
    H1m = pars['H1m']
    N2m = pars['N2m']
    
    h1_imprinting_prob = m_h1
    h2_imprinting_prob = m_h2
    h3_imprinting_prob = m_h3

    # Set imprinting_contribution
    imprinting_contribution_h1 = (1 - H1m * (h1_imprinting_prob + h2_imprinting_prob))
    imprinting_contribution_h3 = (1 - H3m * h3_imprinting_prob) * (1 - N2m * (h3_imprinting_prob + h2_imprinting_prob)) 
    
    # Vaccine effectiveness
    ve_h1 = get_vac_contribution_cohort(pars, p0, 'h1')
    ve_h3 = get_vac_contribution_cohort(pars, p0, 'h3')

    # Calculate age contribution
    age_contribution = get_age_contribution(pars, p0)
    
    baseline_h1 = p0 * imprinting_contribution_h1 * age_contribution * nursing_home_data
    baseline_h3 = p0 * imprinting_contribution_h3 * age_contribution * nursing_home_data
    
    ppH1_unvac = baseline_h1 * age_specific_unvac  * (1 - vacc_coverage_h1)
    ppH1_vac = baseline_h1 * age_specific_vac * vacc_coverage_h1 * (1 - ve_h1)
    
    ppH3_unvac = baseline_h3 * age_specific_unvac  * (1 - vacc_coverage_h3)
    ppH3_vac = baseline_h3 * age_specific_vac * vacc_coverage_h3 * (1 - ve_h3)
    
    h1_norm = rowSums(ppH1_unvac, na.rm=TRUE) + rowSums(ppH1_vac, na.rm=TRUE)
    h3_norm = rowSums(ppH3_unvac, na.rm=TRUE) + rowSums(ppH3_vac, na.rm=TRUE)
    
    ppH1_unvac = ppH1_unvac / h1_norm
    ppH1_vac = ppH1_vac / h1_norm
    
    ppH3_unvac = ppH3_unvac / h3_norm
    ppH3_vac = ppH3_vac / h3_norm
    
    pp = list(ppH1_unvac = ppH1_unvac,
              ppH1_vac = ppH1_vac,
              ppH3_unvac = ppH3_unvac,
              ppH3_vac = ppH3_vac)

    return(pp)
}

########################
#
#      DAHNVcohort subtype
#
########################

DAHNVcohort_subtype = function(pars){
    if (profile){
        for (parameter in names(fixed_pars)){
            pars[parameter] = fixed_pars[[parameter]]
        }
    }

    H3m = pars['H3m']
    H1m = pars['H1m']
    N2m = pars['N2m'] 

    h1_imprinting_prob = m_h1
    h2_imprinting_prob = m_h2
    h3_imprinting_prob = m_h3

    # Set imprinting_contribution
    imprinting_contribution_h1 = (1 - H1m * h1_imprinting_prob)
    imprinting_contribution_h3 = (1 - H3m * h3_imprinting_prob) * (1 - N2m * (h3_imprinting_prob + h2_imprinting_prob)) 
    
    # Vaccine effectiveness
    ve_h1 = get_vac_contribution_cohort(pars, p0, 'h1')
    ve_h3 = get_vac_contribution_cohort(pars, p0, 'h3')

    # Calculate age contribution
    age_contribution = get_age_contribution(pars, p0)
    
    baseline_h1 = p0 * imprinting_contribution_h1 * age_contribution * nursing_home_data
    baseline_h3 = p0 * imprinting_contribution_h3 * age_contribution * nursing_home_data
    
    ppH1_unvac = baseline_h1 * age_specific_unvac  * (1 - vacc_coverage_h1)
    ppH1_vac = baseline_h1 * age_specific_vac * vacc_coverage_h1 * (1 - ve_h1)
    
    ppH3_unvac = baseline_h3 * age_specific_unvac  * (1 - vacc_coverage_h3)
    ppH3_vac = baseline_h3 * age_specific_vac * vacc_coverage_h3 * (1 - ve_h3)
    
    h1_norm = rowSums(ppH1_unvac, na.rm=TRUE) + rowSums(ppH1_vac, na.rm=TRUE)
    h3_norm = rowSums(ppH3_unvac, na.rm=TRUE) + rowSums(ppH3_vac, na.rm=TRUE)
    
    ppH1_unvac = ppH1_unvac / h1_norm
    ppH1_vac = ppH1_vac / h1_norm
    
    ppH3_unvac = ppH3_unvac / h3_norm
    ppH3_vac = ppH3_vac / h3_norm
    
    pp = list(ppH1_unvac = ppH1_unvac,
              ppH1_vac = ppH1_vac,
              ppH3_unvac = ppH3_unvac,
              ppH3_vac = ppH3_vac)

    return(pp)
}

########################
#
#      DAHVage subtype
#
########################

DAHVage_subtype = function(pars){
    if (profile){
        for (parameter in names(fixed_pars)){
            pars[parameter] = fixed_pars[[parameter]]
        }
    }

    H3m = pars['H3m']
    H1m = pars['H1m']

    h1_imprinting_prob = m_h1
    h2_imprinting_prob = m_h2
    h3_imprinting_prob = m_h3
    
    # Set imprinting_contribution
    imprinting_contribution_h1 = (1 - H1m * h1_imprinting_prob) 
    imprinting_contribution_h3 = (1 - H3m * h3_imprinting_prob)
    # Vaccine effectiveness
    ve_h1 = get_vac_contribution_age(pars, p0, 'h1')
    ve_h3 = get_vac_contribution_age(pars, p0, 'h3')

    # Calculate age contribution
    age_contribution = get_age_contribution(pars, p0)
    
    baseline_h1 = p0 * imprinting_contribution_h1 * age_contribution * nursing_home_data
    baseline_h3 = p0 * imprinting_contribution_h3 * age_contribution * nursing_home_data
    
    ppH1_unvac = baseline_h1 * age_specific_unvac  * (1 - vacc_coverage_h1)
    ppH1_vac = baseline_h1 * age_specific_vac * vacc_coverage_h1  * (1 - ve_h1)
    
    ppH3_unvac = baseline_h3 * age_specific_unvac  * (1 - vacc_coverage_h3)
    ppH3_vac = baseline_h3 * age_specific_vac * vacc_coverage_h3 * (1 - ve_h3)
    
    h1_norm = rowSums(ppH1_unvac, na.rm=TRUE) + rowSums(ppH1_vac, na.rm=TRUE)
    h3_norm = rowSums(ppH3_unvac, na.rm=TRUE) + rowSums(ppH3_vac, na.rm=TRUE)
    
    ppH1_unvac = ppH1_unvac / h1_norm
    ppH1_vac = ppH1_vac / h1_norm
    
    ppH3_unvac = ppH3_unvac / h3_norm
    ppH3_vac = ppH3_vac / h3_norm
    
    pp = list(ppH1_unvac = ppH1_unvac,
              ppH1_vac = ppH1_vac,
              ppH3_unvac = ppH3_unvac,
              ppH3_vac = ppH3_vac)

    return(pp)
}


########################
#
#      DAHNVage subtype
#
########################

DAHNVage_subtype = function(pars){
    if (profile){
        for (parameter in names(fixed_pars)){
            pars[parameter] = fixed_pars[[parameter]]
        }
    }

    H3m = pars['H3m']
    H1m = pars['H1m']
    N2m = pars['N2m'] 

    h1_imprinting_prob = m_h1
    h2_imprinting_prob = m_h2
    h3_imprinting_prob = m_h3

    # Set imprinting_contribution
    imprinting_contribution_h1 = (1 - H1m * h1_imprinting_prob)
    imprinting_contribution_h3 = (1 - H3m * h3_imprinting_prob) * (1 - N2m * (h3_imprinting_prob + h2_imprinting_prob)) 
    
    # Vaccine effectiveness
    ve_h1 = get_vac_contribution_age(pars, p0, 'h1')
    ve_h3 = get_vac_contribution_age(pars, p0, 'h3')

    # Calculate age contribution
    age_contribution = get_age_contribution(pars, p0)
    
    baseline_h1 = p0 * imprinting_contribution_h1 * age_contribution * nursing_home_data
    baseline_h3 = p0 * imprinting_contribution_h3 * age_contribution * nursing_home_data
    
    ppH1_unvac = baseline_h1 * age_specific_unvac  * (1 - vacc_coverage_h1)
    ppH1_vac = baseline_h1 * age_specific_vac * vacc_coverage_h1 * (1 - ve_h1)
    
    ppH3_unvac = baseline_h3 * age_specific_unvac  * (1 - vacc_coverage_h3)
    ppH3_vac = baseline_h3 * age_specific_vac * vacc_coverage_h3 * (1 - ve_h3)
    
    h1_norm = rowSums(ppH1_unvac, na.rm=TRUE) + rowSums(ppH1_vac, na.rm=TRUE)
    h3_norm = rowSums(ppH3_unvac, na.rm=TRUE) + rowSums(ppH3_vac, na.rm=TRUE)
    
    ppH1_unvac = ppH1_unvac / h1_norm
    ppH1_vac = ppH1_vac / h1_norm
    
    ppH3_unvac = ppH3_unvac / h3_norm
    ppH3_vac = ppH3_vac / h3_norm
    
    pp = list(ppH1_unvac = ppH1_unvac,
              ppH1_vac = ppH1_vac,
              ppH3_unvac = ppH3_unvac,
              ppH3_vac = ppH3_vac)

    return(pp)
}

########################
#
#      DAHNVage group
#
########################

DAHNVage_group = function(pars){
    if (profile){
        for (parameter in names(fixed_pars)){
            pars[parameter] = fixed_pars[[parameter]]
        }
    }

    H3m = pars['H3m']
    H1m = pars['H1m']
    N2m = pars['N2m']
    
    h1_imprinting_prob = m_h1
    h2_imprinting_prob = m_h2
    h3_imprinting_prob = m_h3

    # Set imprinting_contribution
    imprinting_contribution_h1 = (1 - H1m * (h1_imprinting_prob + h2_imprinting_prob))
    imprinting_contribution_h3 = (1 - H3m * h3_imprinting_prob) * (1 - N2m * (h3_imprinting_prob + h2_imprinting_prob)) 
    
    # Vaccine effectiveness
    ve_h1 = get_vac_contribution_age(pars, p0, 'h1')
    ve_h3 = get_vac_contribution_age(pars, p0, 'h3')

    # Calculate age contribution
    age_contribution = get_age_contribution(pars, p0)
    
    baseline_h1 = p0 * imprinting_contribution_h1 * age_contribution * nursing_home_data
    baseline_h3 = p0 * imprinting_contribution_h3 * age_contribution * nursing_home_data
    
    ppH1_unvac = baseline_h1 * age_specific_unvac  * (1 - vacc_coverage_h1)
    ppH1_vac = baseline_h1 * age_specific_vac * vacc_coverage_h1 * (1 - ve_h1)
    
    ppH3_unvac = baseline_h3 * age_specific_unvac  * (1 - vacc_coverage_h3)
    ppH3_vac = baseline_h3 * age_specific_vac * vacc_coverage_h3 * (1 - ve_h3)
    
    h1_norm = rowSums(ppH1_unvac, na.rm=TRUE) + rowSums(ppH1_vac, na.rm=TRUE)
    h3_norm = rowSums(ppH3_unvac, na.rm=TRUE) + rowSums(ppH3_vac, na.rm=TRUE)
    
    ppH1_unvac = ppH1_unvac / h1_norm
    ppH1_vac = ppH1_vac / h1_norm
    
    ppH3_unvac = ppH3_unvac / h3_norm
    ppH3_vac = ppH3_vac / h3_norm
    
    pp = list(ppH1_unvac = ppH1_unvac,
              ppH1_vac = ppH1_vac,
              ppH3_unvac = ppH3_unvac,
              ppH3_vac = ppH3_vac)

    return(pp)
}

########################
#
#      DAHNVseason subtype
#
########################

DAHNVseason_subtype = function(pars){
    if (profile){
        for (parameter in names(fixed_pars)){
            pars[parameter] = fixed_pars[[parameter]]
        }
    }

    H3m = pars['H3m']
    H1m = pars['H1m']
    N2m = pars['N2m'] 

    h1_imprinting_prob = m_h1
    h2_imprinting_prob = m_h2
    h3_imprinting_prob = m_h3
    
    # Set imprinting_contribution
    imprinting_contribution_h1 = (1 - H1m * h1_imprinting_prob)
    imprinting_contribution_h3 = (1 - H3m * h3_imprinting_prob) * (1 - N2m * (h3_imprinting_prob + h2_imprinting_prob)) 
    
    # Vaccine effectiveness
    ve_h1 = get_vac_contribution(pars, p0)
    ve_h3 = get_vac_contribution(pars, p0)

    # Calculate age contribution
    age_contribution = get_age_contribution(pars, p0)
    
    baseline_h1 = p0 * imprinting_contribution_h1 * age_contribution * nursing_home_data
    baseline_h3 = p0 * imprinting_contribution_h3 * age_contribution * nursing_home_data
    
    ppH1_unvac = baseline_h1 * age_specific_unvac  * (1 - vacc_coverage_h1)
    ppH1_vac = baseline_h1 * age_specific_vac * vacc_coverage_h1 * (1 - ve_h1)
    
    ppH3_unvac = baseline_h3 * age_specific_unvac  * (1 - vacc_coverage_h3)
    ppH3_vac = baseline_h3 * age_specific_vac * vacc_coverage_h3 * (1 - ve_h3)
    
    h1_norm = rowSums(ppH1_unvac, na.rm=TRUE) + rowSums(ppH1_vac, na.rm=TRUE)
    h3_norm = rowSums(ppH3_unvac, na.rm=TRUE) + rowSums(ppH3_vac, na.rm=TRUE)
    
    ppH1_unvac = ppH1_unvac / h1_norm
    ppH1_vac = ppH1_vac / h1_norm
    
    ppH3_unvac = ppH3_unvac / h3_norm
    ppH3_vac = ppH3_vac / h3_norm
    
    pp = list(ppH1_unvac = ppH1_unvac,
              ppH1_vac = ppH1_vac,
              ppH3_unvac = ppH3_unvac,
              ppH3_vac = ppH3_vac)

    return(pp)
}

########################
#
#      DAHNVseason group
#
########################

DAHNVseason_group = function(pars){
    if (profile){
        for (parameter in names(fixed_pars)){
            pars[parameter] = fixed_pars[[parameter]]
        }
    }

    H3m = pars['H3m']
    H1m = pars['H1m']
    N2m = pars['N2m']

    h1_imprinting_prob = m_h1
    h2_imprinting_prob = m_h2
    h3_imprinting_prob = m_h3

    # Set imprinting_contribution
    imprinting_contribution_h1 = (1 - H1m * (h1_imprinting_prob + h2_imprinting_prob))
    imprinting_contribution_h3 = (1 - H3m * h3_imprinting_prob) * (1 - N2m * (h3_imprinting_prob + h2_imprinting_prob)) 
    
    # Vaccine effectiveness
    ve_h1 = get_vac_contribution(pars, p0)
    ve_h3 = get_vac_contribution(pars, p0)

    # Calculate age contribution
    age_contribution = get_age_contribution(pars, p0)
    
    baseline_h1 = p0 * imprinting_contribution_h1 * age_contribution * nursing_home_data
    baseline_h3 = p0 * imprinting_contribution_h3 * age_contribution * nursing_home_data
    
    ppH1_unvac = baseline_h1 * age_specific_unvac  * (1 - vacc_coverage_h1)
    ppH1_vac = baseline_h1 * age_specific_vac * vacc_coverage_h1 * (1 - ve_h1)
    
    ppH3_unvac = baseline_h3 * age_specific_unvac  * (1 - vacc_coverage_h3)
    ppH3_vac = baseline_h3 * age_specific_vac * vacc_coverage_h3 * (1 - ve_h3)
    
    h1_norm = rowSums(ppH1_unvac, na.rm=TRUE) + rowSums(ppH1_vac, na.rm=TRUE)
    h3_norm = rowSums(ppH3_unvac, na.rm=TRUE) + rowSums(ppH3_vac, na.rm=TRUE)
    
    ppH1_unvac = ppH1_unvac / h1_norm
    ppH1_vac = ppH1_vac / h1_norm
    
    ppH3_unvac = ppH3_unvac / h3_norm
    ppH3_vac = ppH3_vac / h3_norm
    
    pp = list(ppH1_unvac = ppH1_unvac,
              ppH1_vac = ppH1_vac,
              ppH3_unvac = ppH3_unvac,
              ppH3_vac = ppH3_vac)

    return(pp)
}

########################
#
#      DAHNVmean subtype
#
########################

DAHNVmean_subtype = function(pars){
    if (profile){
        for (parameter in names(fixed_pars)){
            pars[parameter] = fixed_pars[[parameter]]
        }
    }

    H3m = pars['H3m']
    H1m = pars['H1m']
    N2m = pars['N2m'] 

    h1_imprinting_prob = m_h1
    h2_imprinting_prob = m_h2
    h3_imprinting_prob = m_h3
    
    # Set imprinting_contribution
    imprinting_contribution_h1 = (1 - H1m * h1_imprinting_prob)
    imprinting_contribution_h3 = (1 - H3m * h3_imprinting_prob) * (1 - N2m * (h3_imprinting_prob + h2_imprinting_prob)) 
    
    # Vaccine effectiveness
    ve_h1 = pars['ve_h1']
    ve_h3 = pars['ve_h3']

    # Calculate age contribution
    age_contribution = get_age_contribution(pars, p0)
    
    baseline_h1 = p0 * imprinting_contribution_h1 * age_contribution * nursing_home_data
    baseline_h3 = p0 * imprinting_contribution_h3 * age_contribution * nursing_home_data
    
    ppH1_unvac = baseline_h1 * age_specific_unvac  * (1 - vacc_coverage_h1)
    ppH1_vac = baseline_h1 * age_specific_vac * vacc_coverage_h1 * (1 - ve_h1)
    
    ppH3_unvac = baseline_h3 * age_specific_unvac  * (1 - vacc_coverage_h3)
    ppH3_vac = baseline_h3 * age_specific_vac * vacc_coverage_h3 * (1 - ve_h3)
    
    h1_norm = rowSums(ppH1_unvac, na.rm=TRUE) + rowSums(ppH1_vac, na.rm=TRUE)
    h3_norm = rowSums(ppH3_unvac, na.rm=TRUE) + rowSums(ppH3_vac, na.rm=TRUE)
    
    ppH1_unvac = ppH1_unvac / h1_norm
    ppH1_vac = ppH1_vac / h1_norm
    
    ppH3_unvac = ppH3_unvac / h3_norm
    ppH3_vac = ppH3_vac / h3_norm
    
    pp = list(ppH1_unvac = ppH1_unvac,
              ppH1_vac = ppH1_vac,
              ppH3_unvac = ppH3_unvac,
              ppH3_vac = ppH3_vac)

    return(pp)
}

########################
#
#      DAHNVseason group
#
########################

DAHNVmean_group = function(pars){
    if (profile){
        for (parameter in names(fixed_pars)){
            pars[parameter] = fixed_pars[[parameter]]
        }
    }

    H3m = pars['H3m']
    H1m = pars['H1m']
    N2m = pars['N2m'] 
    
    h1_imprinting_prob = m_h1
    h2_imprinting_prob = m_h2
    h3_imprinting_prob = m_h3

    
    # Set imprinting_contribution
    imprinting_contribution_h1 = (1 - H1m * (h1_imprinting_prob + h2_imprinting_prob))
    imprinting_contribution_h3 = (1 - H3m * h3_imprinting_prob) * (1 - N2m * (h3_imprinting_prob + h2_imprinting_prob)) 
    
    # Vaccine effectiveness
    ve_h1 = pars['ve_h1']
    ve_h3 = pars['ve_h3']

    # Calculate age contribution
    age_contribution = get_age_contribution(pars, p0)
    
    baseline_h1 = p0 * imprinting_contribution_h1 * age_contribution * nursing_home_data
    baseline_h3 = p0 * imprinting_contribution_h3 * age_contribution * nursing_home_data
    
    ppH1_unvac = baseline_h1 * age_specific_unvac  * (1 - vacc_coverage_h1)
    ppH1_vac = baseline_h1 * age_specific_vac * vacc_coverage_h1 * (1 - ve_h1)
    
    ppH3_unvac = baseline_h3 * age_specific_unvac  * (1 - vacc_coverage_h3)
    ppH3_vac = baseline_h3 * age_specific_vac * vacc_coverage_h3 * (1 - ve_h3)
    
    h1_norm = rowSums(ppH1_unvac, na.rm=TRUE) + rowSums(ppH1_vac, na.rm=TRUE)
    h3_norm = rowSums(ppH3_unvac, na.rm=TRUE) + rowSums(ppH3_vac, na.rm=TRUE)
    
    ppH1_unvac = ppH1_unvac / h1_norm
    ppH1_vac = ppH1_vac / h1_norm
    
    ppH3_unvac = ppH3_unvac / h3_norm
    ppH3_vac = ppH3_vac / h3_norm
    
    pp = list(ppH1_unvac = ppH1_unvac,
              ppH1_vac = ppH1_vac,
              ppH3_unvac = ppH3_unvac,
              ppH3_vac = ppH3_vac)

    return(pp)
}

########################
#
#      DAHNVimprinting subtype
#
########################

DAHNVimprinting_subtype = function(pars){
    if (profile){
        for (parameter in names(fixed_pars)){
            pars[parameter] = fixed_pars[[parameter]]
        }
    }
    
    H3m = pars['H3m']
    H1m = pars['H1m']
    N2m = pars['N2m']

    VE_h1_h1 = pars['VE_h1_h1']
    VE_h1_h2 = pars['VE_h1_h2']
    VE_h1_h3 = pars['VE_h1_h3']

    VE_h3_h1 = pars['VE_h3_h1']
    VE_h3_h2 = pars['VE_h3_h2']
    VE_h3_h3 = pars['VE_h3_h3']

    h1_imprinting_prob = m_h1
    h2_imprinting_prob = m_h2
    h3_imprinting_prob = m_h3

    # Set imprinting_contribution
    imprinting_contribution_h1 = (1 - H1m * h1_imprinting_prob) 
    imprinting_contribution_h3 = (1 - H3m * h3_imprinting_prob) * (1 - N2m * (h3_imprinting_prob + h2_imprinting_prob)) 

    vac_contribution_h1 = (1 - VE_h1_h1 * h1_imprinting_prob) * 
                          (1 - VE_h1_h2 * h2_imprinting_prob) * 
                          (1 - VE_h1_h3 * h3_imprinting_prob)

    vac_contribution_h3 = (1 - VE_h3_h1 * h1_imprinting_prob) *
                          (1 - VE_h3_h2 * h2_imprinting_prob) *
                          (1 - VE_h3_h3 * h3_imprinting_prob)

    
    # Calculate age contribution
    age_contribution = get_age_contribution(pars, p0)
    
    baseline_h1 = p0 * imprinting_contribution_h1 * age_contribution * nursing_home_data
    baseline_h3 = p0 * imprinting_contribution_h3 * age_contribution * nursing_home_data
    
    ppH1_unvac = baseline_h1 * age_specific_unvac  * (1 - vacc_coverage_h1)
    ppH1_vac = baseline_h1 * age_specific_vac * vacc_coverage_h1 * vac_contribution_h1
    
    ppH3_unvac = baseline_h3 * age_specific_unvac  * (1 - vacc_coverage_h3)
    ppH3_vac = baseline_h3 * age_specific_vac * vacc_coverage_h3 * vac_contribution_h3
    
    h1_norm = rowSums(ppH1_unvac, na.rm=TRUE) + rowSums(ppH1_vac, na.rm=TRUE)
    h3_norm = rowSums(ppH3_unvac, na.rm=TRUE) + rowSums(ppH3_vac, na.rm=TRUE)
    
    ppH1_unvac = ppH1_unvac / h1_norm
    ppH1_vac = ppH1_vac / h1_norm
    
    ppH3_unvac = ppH3_unvac / h3_norm
    ppH3_vac = ppH3_vac / h3_norm
    
    pp = list(ppH1_unvac = ppH1_unvac,
              ppH1_vac = ppH1_vac,
              ppH3_unvac = ppH3_unvac,
              ppH3_vac = ppH3_vac)
    return(pp)
}

########################
#
#      DAHNVimprinting group
#
########################

DAHNVimprinting_group = function(pars){
    if (profile){
        for (parameter in names(fixed_pars)){
            pars[parameter] = fixed_pars[[parameter]]
        }
    }
    
    H3m = pars['H3m']
    H1m = pars['H1m']
    N2m = pars['N2m']

    VE_h1_h1 = pars['VE_h1_h1']
    VE_h1_h3 = pars['VE_h1_h3']

    VE_h3_h1 = pars['VE_h3_h1']
    VE_h3_h3 = pars['VE_h3_h3']

    h1_imprinting_prob = m_h1
    h2_imprinting_prob = m_h2
    h3_imprinting_prob = m_h3

    # Set imprinting_contribution
    imprinting_contribution_h1 = (1 - H1m * (h1_imprinting_prob + h2_imprinting_prob))
    imprinting_contribution_h3 = (1 - H3m * h3_imprinting_prob) * (1 - N2m * (h3_imprinting_prob + h2_imprinting_prob)) 

    vac_contribution_h1 = (1 - VE_h1_h1 * (h1_imprinting_prob + h2_imprinting_prob)) * 
                          (1 - VE_h1_h3 * h3_imprinting_prob)

    vac_contribution_h3 = (1 - VE_h3_h1 * (h1_imprinting_prob + h2_imprinting_prob)) *
                          (1 - VE_h3_h3 * h3_imprinting_prob)

    
    # Calculate age contribution
    age_contribution = get_age_contribution(pars, p0)
    
    baseline_h1 = p0 * imprinting_contribution_h1 * age_contribution * nursing_home_data
    baseline_h3 = p0 * imprinting_contribution_h3 * age_contribution * nursing_home_data
    
    ppH1_unvac = baseline_h1 * age_specific_unvac  * (1 - vacc_coverage_h1)
    ppH1_vac = baseline_h1 * age_specific_vac * vacc_coverage_h1 * vac_contribution_h1
    
    ppH3_unvac = baseline_h3 * age_specific_unvac  * (1 - vacc_coverage_h3)
    ppH3_vac = baseline_h3 * age_specific_vac * vacc_coverage_h3 * vac_contribution_h3
    
    h1_norm = rowSums(ppH1_unvac, na.rm=TRUE) + rowSums(ppH1_vac, na.rm=TRUE)
    h3_norm = rowSums(ppH3_unvac, na.rm=TRUE) + rowSums(ppH3_vac, na.rm=TRUE)
    
    ppH1_unvac = ppH1_unvac / h1_norm
    ppH1_vac = ppH1_vac / h1_norm
    
    ppH3_unvac = ppH3_unvac / h3_norm
    ppH3_vac = ppH3_vac / h3_norm
    
    pp = list(ppH1_unvac = ppH1_unvac,
              ppH1_vac = ppH1_vac,
              ppH3_unvac = ppH3_unvac,
              ppH3_vac = ppH3_vac)
    return(pp)
}

########################
#
#      DAHNVageseason subtype
#
########################

DAHNVageseason_subtype = function(pars){
    if (profile){
        for (parameter in names(fixed_pars)){
            pars[parameter] = fixed_pars[[parameter]]
        }
    }

    H3m = pars['H3m']
    H1m = pars['H1m']
    N2m = pars['N2m'] 

    h1_imprinting_prob = m_h1
    h2_imprinting_prob = m_h2
    h3_imprinting_prob = m_h3

    # Set imprinting_contribution
    imprinting_contribution_h1 = (1 - H1m * h1_imprinting_prob)
    imprinting_contribution_h3 = (1 - H3m * h3_imprinting_prob) * (1 - N2m * (h3_imprinting_prob + h2_imprinting_prob)) 
    
    # Vaccine effectiveness
    ve_h1 = ve_h3 = get_vac_contribution_ageseason(pars, p0)

    # Calculate age contribution
    age_contribution = get_age_contribution(pars, p0)
    
    baseline_h1 = p0 * imprinting_contribution_h1 * age_contribution * nursing_home_data
    baseline_h3 = p0 * imprinting_contribution_h3 * age_contribution * nursing_home_data
    
    ppH1_unvac = baseline_h1 * age_specific_unvac  * (1 - vacc_coverage_h1)
    ppH1_vac = baseline_h1 * age_specific_vac * vacc_coverage_h1 * (1 - ve_h1)
    
    ppH3_unvac = baseline_h3 * age_specific_unvac  * (1 - vacc_coverage_h3)
    ppH3_vac = baseline_h3 * age_specific_vac * vacc_coverage_h3 * (1 - ve_h3)
    
    h1_norm = rowSums(ppH1_unvac, na.rm=TRUE) + rowSums(ppH1_vac, na.rm=TRUE)
    h3_norm = rowSums(ppH3_unvac, na.rm=TRUE) + rowSums(ppH3_vac, na.rm=TRUE)
    
    ppH1_unvac = ppH1_unvac / h1_norm
    ppH1_vac = ppH1_vac / h1_norm
    
    ppH3_unvac = ppH3_unvac / h3_norm
    ppH3_vac = ppH3_vac / h3_norm
    
    pp = list(ppH1_unvac = ppH1_unvac,
              ppH1_vac = ppH1_vac,
              ppH3_unvac = ppH3_unvac,
              ppH3_vac = ppH3_vac)

    return(pp)
}