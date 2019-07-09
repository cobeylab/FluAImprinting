########################
#
#      DAHVage subtype vaccination blocks imprinting
#
########################

DAHVage_subtype_vProtect_x = function(pars){
    if (profile){
        pars[par_name] = par_test
    }

    H3m = pars['H3m']
    H1m = pars['H1m']

    h1_imprinting_prob = m_h1_v
    h2_imprinting_prob = m_h2_v
    h3_imprinting_prob = m_h3_v
    
    # Set imprinting_contribution
    imprinting_contribution_h1 = (1 - H1m * h1_imprinting_prob) * (1 - H1m * x * (m_v + m_v_mono))
    imprinting_contribution_h3 = (1 - H3m * h3_imprinting_prob) * (1 - H3m * x * m_v)
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