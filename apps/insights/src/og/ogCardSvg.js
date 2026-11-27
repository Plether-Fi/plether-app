export const OG_CARD_WIDTH = 1200;
export const OG_CARD_HEIGHT = 630;

const OFFICIAL_BADGE_PATHS = {
  iron: `<path d="M165.577 82.7887C165.577 128.512 128.512 165.577 82.7887 165.577C37.0658 165.577 0 128.512 0 82.7887C0 37.0658 37.0658 0 82.7887 0C128.512 0 165.577 37.0658 165.577 82.7887Z" fill="#250917"/>
<path d="M82.7887 50.2651L90.066 72.6593L113.613 72.6602L94.5636 86.5014L101.839 108.896L82.7887 95.0563L63.7384 108.896L71.0139 86.5014L51.9646 72.6602L75.5115 72.6593L82.7887 50.2651Z" fill="#F2CF71"/>
<path d="M93.3705 68.1105L127.611 68.1121L99.9108 88.2389L110.49 120.804L82.7887 100.679L55.0871 120.804L65.6664 88.2389L37.9663 68.1121L72.2067 68.1105L82.7887 35.5466L93.3705 68.1105ZM75.5115 72.6594L51.9646 72.6603L71.014 86.5015L63.7383 108.896L82.7887 95.0564L101.839 108.896L94.5635 86.5015L113.613 72.6603L90.066 72.6594L82.7887 50.2653L75.5115 72.6594Z" fill="#FFAB96"/>
<path d="M82.5613 64.8213L86.5191 77.0791L99.3253 77.0796L88.9652 84.6559L92.922 96.9141L82.5613 89.3386L72.2006 96.9141L76.1575 84.6559L65.7973 77.0796L78.6035 77.0791L82.5613 64.8213Z" fill="#FFAB96"/>
<path d="M86.5191 77.0791L99.3253 77.0796L88.9652 84.6558L92.9221 96.9141L82.5613 89.3386L72.2005 96.9141L76.1574 84.6558L65.7972 77.0796L78.6035 77.0791L82.5613 64.8213L86.5191 77.0791ZM80.2591 79.3536H72.76L78.8267 83.7902L76.5192 90.9386L82.5613 86.5211L88.6031 90.9386L86.2959 83.7902L92.3624 79.3536H84.8635L82.5613 72.2236L80.2591 79.3536Z" fill="#250917"/>`,

  bronze: `<path d="M165.577 82.7887C165.577 128.512 128.512 165.577 82.7887 165.577C37.0658 165.577 0 128.512 0 82.7887C0 37.0658 37.0658 0 82.7887 0C128.512 0 165.577 37.0658 165.577 82.7887Z" fill="#250917"/>
<path d="M82.9024 39.1201L93.4266 64.5605L120.721 60.9545L103.951 82.7889L120.721 104.623L93.4266 101.017L82.9024 126.458L72.3783 101.017L45.0842 104.623L61.8541 82.7889L45.0842 60.9545L72.3783 64.5605L82.9024 39.1201Z" fill="#F2CF71"/>
<path d="M96.2944 59.5924L131.026 55.004L109.686 82.7883L131.026 110.573L96.2944 105.984L82.9024 138.357L69.5103 105.984L34.7788 110.573L56.1183 82.7883L34.7788 55.004L69.5103 59.5924L82.9024 27.2197L96.2944 59.5924ZM72.3782 64.5599L45.0841 60.9539L61.8541 82.7883L45.0841 104.623L72.3782 101.017L82.9024 126.457L93.4267 101.017L120.721 104.623L103.951 82.7883L120.721 60.9539L93.4267 64.5599L82.9024 39.1195L72.3782 64.5599Z" fill="#FFAB96"/>
<path d="M82.9025 55.2681L89.5349 71.2346L106.736 68.9714L96.1673 82.6748L106.736 96.3781L89.5349 94.115L82.9025 110.081L76.27 94.115L59.0691 96.3781L69.6376 82.6748L59.0691 68.9714L76.27 71.2346L82.9025 55.2681Z" fill="#FF572D"/>
<path d="M89.5349 71.2345L106.736 68.9714L96.1674 82.6748L106.736 96.3781L89.5349 94.115L82.9025 110.081L76.27 94.115L59.0691 96.3781L69.6375 82.6748L59.0691 68.9714L76.27 71.2345L82.9025 55.2681L89.5349 71.2345ZM77.7017 73.717L64.2349 71.945L72.5099 82.6748L64.2347 93.4043L77.7017 91.6327L82.9025 104.152L88.1032 91.6327L101.57 93.4043L93.295 82.6748L101.57 71.945L88.1032 73.717L82.9025 61.1971L77.7017 73.717Z" fill="#250917"/>
<path d="M88.816 82.7885C88.816 86.0544 86.2193 88.702 83.0162 88.702C79.8131 88.702 77.2164 86.0544 77.2164 82.7885C77.2164 79.5226 79.8131 76.875 83.0162 76.875C86.2193 76.875 88.816 79.5226 88.816 82.7885Z" fill="#F2CF71"/>
<path d="M86.5415 82.7885C86.5415 80.7368 84.9218 79.1494 83.0162 79.1494C81.1106 79.1494 79.4908 80.7368 79.4908 82.7885C79.4908 84.8401 81.1106 86.4275 83.0162 86.4275V88.702C79.8131 88.702 77.2164 86.0544 77.2164 82.7885C77.2164 79.5226 79.8131 76.875 83.0162 76.875C86.2193 76.875 88.816 79.5226 88.816 82.7885C88.816 86.0544 86.2193 88.702 83.0162 88.702V86.4275C84.9218 86.4275 86.5415 84.8401 86.5415 82.7885Z" fill="#FFAB96"/>`,

  silver: `<path d="M165.577 82.7887C165.577 128.512 128.512 165.577 82.7887 165.577C37.0658 165.577 0 128.512 0 82.7887C0 37.0658 37.0658 0 82.7887 0C128.512 0 165.577 37.0658 165.577 82.7887Z" fill="#250917"/>
<path d="M82.9025 35.481L94.8774 58.0364L119.978 53.3357L109.81 76.7611L129.135 93.4548L104.481 100.11L103.478 125.628L82.9025 110.502L62.327 125.628L61.3244 100.11L36.6699 93.4548L55.9951 76.7611L45.8268 53.3357L70.9276 58.0364L82.9025 35.481Z" fill="#FFAB96"/>
<path d="M97.3268 52.9495L127.562 47.2875L115.314 75.5045L138.593 95.6135L108.894 103.631L107.687 134.368L82.9025 116.148L58.118 134.368L56.9104 103.63L27.2124 95.6135L50.4907 75.5045L38.2426 47.2875L68.4779 52.9495L82.9025 25.7803L97.3268 52.9495ZM70.9276 58.0363L45.8268 53.3356L55.9951 76.7612L36.6699 93.4548L61.3244 100.11L62.327 125.628L82.9025 110.502L103.478 125.628L104.481 100.11L129.135 93.4548L109.81 76.7612L119.978 53.3356L94.8773 58.0363L82.9025 35.481L70.9276 58.0363Z" fill="#FF572D"/>
<path d="M82.7887 57.0879L89.2787 69.3122L102.883 66.7645L97.3716 79.4603L107.845 88.5078L94.4833 92.1149L93.9399 105.944L82.7887 97.7467L71.6375 105.944L71.0942 92.1149L57.7322 88.5078L68.2058 79.4603L62.695 66.7645L76.2987 69.3122L82.7887 57.0879Z" fill="#F2CF71"/>
<path d="M89.2788 69.3122L102.882 66.7646L97.3716 79.4602L107.845 88.5077L94.4833 92.115L93.94 105.945L82.7887 97.7466L71.6374 105.945L71.0941 92.115L57.7322 88.5077L68.2058 79.4602L62.695 66.7646L76.2987 69.3122L82.7887 57.0879L89.2788 69.3122ZM77.5236 71.8556L66.4871 69.7886L70.958 80.0886L62.4609 87.4282L73.3013 90.3548L73.7419 101.574L82.7887 94.9238L91.8353 101.574L92.2762 90.3548L103.116 87.4282L94.6195 80.0886L99.0901 69.7886L88.0539 71.8556L82.7887 61.9381L77.5236 71.8556Z" fill="#250917"/>`,

  gold: `<path d="M165.577 82.7887C165.577 128.512 128.512 165.577 82.7888 165.577C37.0658 165.577 3.05176e-05 128.512 3.05176e-05 82.7887C3.05176e-05 37.0658 37.0658 0 82.7888 0C128.512 0 165.577 37.0658 165.577 82.7887Z" fill="#250917"/>
<path d="M82.9025 32.979L95.932 51.4464L118.204 47.6012L114.359 69.8729L132.826 82.9024L114.359 95.9319L118.204 118.204L95.932 114.358L82.9025 132.826L69.8729 114.358L47.6013 118.204L51.4464 95.9319L32.979 82.9024L51.4464 69.8729L47.6013 47.6012L69.8729 51.4464L82.9025 32.979Z" fill="#FFAB96"/>
<path d="M97.9911 46.4743L123.783 42.0217L119.33 67.8134L140.716 82.9023L119.33 97.991L123.783 123.783L97.9911 119.33L82.9025 140.716L67.8136 119.33L42.0219 123.783L46.4745 97.991L25.0886 82.9023L46.4745 67.8134L42.0219 42.0217L67.8136 46.4743L82.9025 25.0884L97.9911 46.4743ZM69.873 51.4463L47.6013 47.6011L51.4465 69.8728L32.9791 82.9023L51.4465 95.9318L47.6013 118.203L69.873 114.358L82.9025 132.826L95.9319 114.358L118.204 118.203L114.358 95.9318L132.826 82.9023L114.358 69.8728L118.204 47.6011L95.9319 51.4463L82.9025 32.9789L69.873 51.4463Z" fill="#FF572D"/>
<path d="M82.7887 46.1704L92.3457 59.716L108.682 56.8956L105.861 73.2315L119.407 82.7885L105.861 92.3455L108.682 108.681L92.3457 105.861L82.7887 119.407L73.2318 105.861L56.8958 108.681L59.7162 92.3455L46.1707 82.7885L59.7162 73.2315L56.8958 56.8956L73.2318 59.716L82.7887 46.1704Z" fill="#F2CF71"/>
<path d="M92.3457 59.7161L108.682 56.8957L105.861 73.2315L119.407 82.7885L105.861 92.3455L108.682 108.681L92.3457 105.861L82.7887 119.407L73.2318 105.861L56.8959 108.681L59.7163 92.3455L46.1707 82.7885L59.7163 73.2315L56.8959 56.8957L73.2318 59.7161L82.7887 46.1704L92.3457 59.7161ZM74.2615 62.2017L59.6854 59.6852L62.202 74.2612L50.1158 82.7885L62.202 91.3158L59.6854 105.892L74.2615 103.375L82.7887 115.461L91.316 103.375L105.892 105.892L103.376 91.3158L115.461 82.7885L103.376 74.2612L105.892 59.6852L91.316 62.2017L82.7887 50.1155L74.2615 62.2017Z" fill="#250917"/>
<path d="M82.9025 60.7271L88.6901 68.9301L98.583 67.2221L96.875 77.115L105.078 82.9026L96.875 88.6902L98.583 98.5831L88.6901 96.8751L82.9025 105.078L77.1149 96.8751L67.222 98.5831L68.93 88.6902L60.7269 82.9026L68.93 77.115L67.222 67.2221L77.1149 68.9301L82.9025 60.7271Z" fill="#FF572D"/>
<path d="M88.69 68.9301L98.5831 67.222L96.875 77.1151L105.078 82.9026L96.875 88.6901L98.5831 98.5832L88.69 96.8752L82.9025 105.078L77.1149 96.8752L67.2219 98.5832L68.9299 88.6901L60.7269 82.9026L68.9299 77.1151L67.2219 67.222L77.1149 68.9301L82.9025 60.7271L88.69 68.9301ZM78.1446 71.4159L70.0116 70.0117L71.4158 78.1448L64.6721 82.9026L71.4158 87.6604L70.0116 95.7933L78.1446 94.3893L82.9025 101.133L87.6603 94.3893L95.7931 95.7933L94.3892 87.6604L101.133 82.9026L94.3892 78.1448L95.7931 70.0117L87.6603 71.4159L82.9025 64.6722L78.1446 71.4159Z" fill="#FFAB96"/>
<path d="M89.612 82.9029C89.612 86.6085 86.608 89.6124 82.9025 89.6124C79.1969 89.6124 76.1929 86.6085 76.1929 82.9029C76.1929 79.1973 79.1969 76.1934 82.9025 76.1934C86.608 76.1934 89.612 79.1973 89.612 82.9029Z" fill="#250917"/>
<path d="M87.3376 82.9029C87.3376 80.4534 85.3519 78.4678 82.9025 78.4678C80.453 78.4678 78.4673 80.4534 78.4673 82.9029C78.4673 85.3523 80.453 87.338 82.9025 87.338V89.6124C79.1969 89.6124 76.1929 86.6085 76.1929 82.9029C76.1929 79.1973 79.1969 76.1934 82.9025 76.1934C86.608 76.1934 89.612 79.1973 89.612 82.9029C89.612 86.6085 86.608 89.6124 82.9025 89.6124V87.338C85.3519 87.338 87.3376 85.3523 87.3376 82.9029Z" fill="#F2CF71"/>`,

  platinum: `<path d="M165.577 82.7887C165.577 128.512 128.512 165.577 82.7887 165.577C37.0658 165.577 0 128.512 0 82.7887C0 37.0658 37.0658 0 82.7887 0C128.512 0 165.577 37.0658 165.577 82.7887Z" fill="#250917"/>
<path d="M82.9025 26.1558L98.0495 41.1726L119.305 39.4054L121.256 60.6452L138.675 72.9545L126.517 90.4791L131.948 111.105L111.37 116.715L102.272 136.006L82.9025 127.076L63.5328 136.006L54.4353 116.715L33.8569 111.105L39.2883 90.4791L27.1299 72.9545L44.5488 60.6452L46.4995 39.4054L67.7554 41.1726L82.9025 26.1558Z" fill="#FF572D"/>
<path d="M99.7624 36.4657L123.423 34.4987L125.594 58.1408L144.983 71.8424L131.449 91.3488L137.495 114.308L114.589 120.552L104.463 142.026L82.9024 132.085L61.3421 142.026L51.2154 120.552L28.3096 114.308L34.3552 91.3488L20.8218 71.8424L40.2107 58.1408L42.3821 34.4987L66.0422 36.4657L82.9024 19.7505L99.7624 36.4657ZM67.7554 41.1729L46.4996 39.4055L44.5488 60.6453L27.1299 72.9547L39.2883 90.4793L33.8568 111.105L54.4354 116.715L63.5328 136.006L82.9024 127.076L102.272 136.006L111.37 116.715L131.948 111.105L126.517 90.4793L138.675 72.9547L121.256 60.6453L119.305 39.4055L98.0495 41.1729L82.9024 26.1559L67.7554 41.1729Z" fill="#F2CF71"/>
<path d="M82.7888 44.1235L93.0997 54.376L107.569 53.1694L108.897 67.6706L120.754 76.0745L112.478 88.039L116.175 102.121L102.167 105.951L95.9741 119.122L82.7888 113.025L69.6034 119.122L63.4105 105.951L49.4023 102.121L53.0996 88.039L44.8231 76.0745L56.6806 67.6706L58.0084 53.1694L72.4778 54.376L82.7888 44.1235Z" fill="#FFAB96"/>
<path d="M93.0996 54.376L107.569 53.1694L108.897 67.6706L120.754 76.0744L112.478 88.0391L116.175 102.121L102.167 105.951L95.9741 119.122L82.7887 113.025L69.6033 119.122L63.4104 105.951L49.4024 102.121L53.0996 88.0391L44.8231 76.0744L56.6805 67.6706L58.0085 53.1694L72.4779 54.376L82.7887 44.1235L93.0996 54.376ZM73.3361 56.7299L60.0675 55.6233L58.8501 68.9207L47.9726 76.6297L55.565 87.6051L52.1739 100.521L65.0223 104.034L70.6999 116.109L82.7887 110.519L94.8773 116.109L100.555 104.034L113.403 100.521L110.012 87.6051L117.605 76.6297L106.727 68.9207L105.51 55.6233L92.2413 56.7299L82.7887 47.3308L73.3361 56.7299Z" fill="#250917"/>
<path d="M82.675 60.272L87.7308 75.7514L104.09 75.7521L90.8554 85.3195L95.91 100.799L82.675 91.2329L69.44 100.799L74.4946 85.3195L61.2603 75.7521L77.6192 75.7514L82.675 60.272Z" fill="#F2CF71"/>
<path d="M87.7307 75.7515L104.09 75.752L90.8554 85.3194L95.9099 100.799L82.675 91.2329L69.4401 100.799L74.4947 85.3194L61.2604 75.752L77.6193 75.7515L82.675 60.272L87.7307 75.7515ZM79.2692 78.0257L68.288 78.0262L77.1718 84.4483L73.771 94.8622L82.675 88.4265L91.5788 94.8622L88.1783 84.4483L97.0618 78.0262L86.0809 78.0257L82.675 67.5976L79.2692 78.0257Z" fill="#FF572D"/>`,

  diamond: `<path d="M165.577 82.7887C165.577 128.512 128.512 165.577 82.7887 165.577C37.0658 165.577 0 128.512 0 82.7887C0 37.0658 37.0658 0 82.7887 0C128.512 0 165.577 37.0658 165.577 82.7887Z" fill="#250917"/>
<path d="M82.9025 8.64307L99.8543 19.6375L120.032 18.592L129.216 36.5893L147.213 45.7729L146.168 65.9509L157.162 82.9027L146.168 99.8546L147.213 120.033L129.216 129.216L120.032 147.214L99.8543 146.168L82.9025 157.162L65.9506 146.168L45.7726 147.214L36.5891 129.216L18.5917 120.033L19.6372 99.8546L8.64279 82.9027L19.6372 65.9509L18.5917 45.7729L36.5891 36.5893L45.7726 18.592L65.9506 19.6375L82.9025 8.64307Z" fill="#FF572D"/>
<path d="M101.092 15.0177L122.743 13.896L132.597 33.2072L151.909 43.0614L150.787 64.7123L162.584 82.9021L150.787 101.092L151.909 122.743L132.597 132.597L122.743 151.908L101.092 150.786L82.9025 162.584L64.7127 150.786L43.0618 151.908L33.2076 132.597L13.8964 122.743L15.018 101.092L3.22107 82.9021L15.018 64.7123L13.8964 43.0614L33.2076 33.2072L43.0618 13.896L64.7127 15.0177L82.9025 3.2207L101.092 15.0177ZM65.9505 19.6369L45.7726 18.5914L36.589 36.5886L18.5918 45.7723L19.6373 65.9502L8.6428 82.9021L19.6373 99.8541L18.5918 120.032L36.589 129.216L45.7726 147.213L65.9505 146.167L82.9025 157.162L99.8544 146.167L120.032 147.213L129.216 129.216L147.213 120.032L146.168 99.8541L157.162 82.9021L146.168 65.9502L147.213 45.7723L129.216 36.5886L120.032 18.5914L99.8544 19.6369L82.9025 8.64243L65.9505 19.6369Z" fill="#F2CF71"/>
<path d="M82.9025 25.7007L96.0123 34.1696L111.617 33.3642L118.719 47.2274L132.637 54.3015L131.829 69.8444L140.331 82.9022L131.829 95.9601L132.637 111.503L118.719 118.577L111.617 132.44L96.0123 131.635L82.9025 140.104L69.7927 131.635L54.188 132.44L47.0858 118.577L33.1675 111.503L33.976 95.9601L25.4735 82.9022L33.976 69.8444L33.1675 54.3015L47.0858 47.2274L54.188 33.3642L69.7927 34.1696L82.9025 25.7007Z" fill="#FFAB96"/>
<path d="M96.0124 34.1696L111.617 33.3642L118.719 47.2275L132.638 54.3015L131.829 69.8443L140.331 82.9022L131.829 95.9601L132.638 111.503L118.719 118.577L111.617 132.44L96.0124 131.635L82.9025 140.104L69.7926 131.635L54.188 132.44L47.0858 118.577L33.1674 111.503L33.9761 95.9601L25.4735 82.9022L33.9761 69.8443L33.1674 54.3015L47.0858 47.2275L54.188 33.3642L69.7926 34.1696L82.9025 25.7007L96.0124 34.1696ZM70.4094 36.4788L55.5409 35.7114L48.7738 48.9208L35.5156 55.6592L36.2858 70.4654L28.1875 82.9022L36.2858 95.3391L35.5156 110.145L48.7738 116.884L55.5409 130.093L70.4094 129.326L82.9025 137.396L95.3956 129.326L110.264 130.093L117.031 116.884L130.289 110.145L129.519 95.3391L137.617 82.9022L129.519 70.4654L130.289 55.6592L117.031 48.9208L110.264 35.7114L95.3956 36.4788L82.9025 28.4082L70.4094 36.4788Z" fill="#250917"/>
<path d="M82.7887 47.5352L92.3351 53.2169L103.376 54.268L107.782 64.5123L116.1 71.8947L113.682 82.7886L116.1 93.6825L107.782 101.065L103.376 111.309L92.3351 112.36L82.7887 118.042L73.2423 112.36L62.2009 111.309L57.7958 101.065L49.477 93.6825L51.8958 82.7886L49.477 71.8947L57.7958 64.5123L62.2009 54.268L73.2423 53.2169L82.7887 47.5352Z" fill="#F2CF71"/>
<path d="M92.3353 53.217L103.376 54.268L107.782 64.5122L116.1 71.8947L113.682 82.7886L116.1 93.6825L107.782 101.065L103.376 111.309L92.3353 112.36L82.7887 118.042L73.2422 112.36L62.2011 111.309L57.7959 101.065L49.477 93.6825L51.8958 82.7886L49.477 71.8947L57.7959 64.5122L62.2011 54.268L73.2422 53.217L82.7887 47.5352L92.3353 53.217ZM73.9665 55.4328L63.7581 56.4045L59.685 65.8766L51.9873 72.7076L54.2255 82.7886L51.9873 92.8693L59.685 99.7006L63.7581 109.172L73.9665 110.144L82.7887 115.395L91.611 110.144L101.819 109.172L105.893 99.7006L113.59 92.8693L111.352 82.7886L113.59 72.7076L105.893 65.8766L101.819 56.4045L91.611 55.4328L82.7887 50.1816L73.9665 55.4328Z" fill="#FF572D"/>
<path d="M82.7887 60.0444L87.1763 75.3404L102.683 71.4734L91.5639 82.9023L102.683 94.3313L87.1763 90.4642L82.7887 105.76L78.4011 90.4642L62.8947 94.3313L74.0136 82.9023L62.8947 71.4734L78.4011 75.3404L82.7887 60.0444Z" fill="#FFAB96"/>
<path d="M87.1763 75.3403L102.683 71.4734L91.5639 82.9023L102.683 94.3313L87.1763 90.4643L82.7887 105.76L78.4012 90.4643L62.8947 94.3313L74.0136 82.9023L62.8947 71.4734L78.4012 75.3403L82.7887 60.0444L87.1763 75.3403ZM79.9819 78.0787L70.0955 75.6131L77.1867 82.9023L70.0955 90.1913L79.9819 87.7259L82.7887 97.5114L85.5956 87.7259L95.4817 90.1913L88.3908 82.9023L95.4817 75.6131L85.5956 78.0787L82.7887 68.293L79.9819 78.0787Z" fill="#250917"/>`,
};

const TIER_COLORS = {
  iron: { primary: '#FFAB96', secondary: '#F2CF71', glow: '#FFAB96' },
  bronze: { primary: '#F2CF71', secondary: '#FF572D', glow: '#FFAB96' },
  silver: { primary: '#FFAB96', secondary: '#FF572D', glow: '#FF572D' },
  gold: { primary: '#F2CF71', secondary: '#FF572D', glow: '#F2CF71' },
  platinum: { primary: '#F2CF71', secondary: '#FF572D', glow: '#FF572D' },
  diamond: { primary: '#F2CF71', secondary: '#FF572D', glow: '#FF572D' },
};

const DEFAULT_TIER_PALETTE = { primary: '#FFAB96', secondary: '#FF572D', glow: '#FF572D' };
const SANS_FONT = 'Uncut Sans, sans-serif';
const MONO_FONT = 'JetBrains Mono, Uncut Sans, monospace';

export function escapeXml(value) {
  return String(value ?? '')
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;')
    .replaceAll("'", '&apos;');
}

export function shortAddress(address) {
  if (typeof address !== 'string' || address.length < 12) return String(address ?? '');
  return `${address.slice(0, 6)}...${address.slice(-4)}`;
}

function resolveTierPalette(tier) {
  if (!tier) return DEFAULT_TIER_PALETTE;
  return TIER_COLORS[String(tier).trim().toLowerCase()] ?? DEFAULT_TIER_PALETTE;
}

function getOfficialBadgePaths(tier) {
  if (!tier) return null;
  const key = String(tier).trim().toLowerCase();
  return Object.prototype.hasOwnProperty.call(OFFICIAL_BADGE_PATHS, key)
    ? OFFICIAL_BADGE_PATHS[key]
    : null;
}

export function formatUsdgAssets(raw) {
  if (typeof raw !== 'string' || !/^-?\d+$/.test(raw)) return 'Unavailable';
  const value = BigInt(raw);
  const neg = value < 0n;
  const abs = neg ? -value : value;
  const whole = abs / 1_000_000n;
  const frac = (abs % 1_000_000n).toString().padStart(6, '0').slice(0, 2);
  const formattedWhole = whole.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ',');
  return `${neg ? '−' : ''}${formattedWhole}.${frac} USDG`;
}

export function formatCompactUsdg(raw) {
  if (typeof raw !== 'string' || !/^-?\d+$/.test(raw)) return 'Unavailable';
  const num = Number(BigInt(raw)) / 1e6;
  if (!Number.isFinite(num)) return 'Unavailable';
  const abs = Math.abs(num);
  const sign = num < 0 ? '−' : '';
  if (abs >= 1e9) return `${sign}${(abs / 1e9).toFixed(2)}B USDG`;
  if (abs >= 1e6) return `${sign}${(abs / 1e6).toFixed(2)}M USDG`;
  if (abs >= 1e3) return `${sign}${(abs / 1e3).toFixed(2)}K USDG`;
  return `${sign}${abs.toFixed(2)} USDG`;
}

export function formatSignedUsdg(raw) {
  if (typeof raw !== 'string' || !/^-?\d+$/.test(raw)) {
    return { text: 'Unavailable', tone: 'neutral' };
  }
  const value = BigInt(raw);
  if (value > 0n) return { text: `+${formatUsdgAssets(value.toString())}`, tone: 'positive' };
  if (value < 0n) return { text: `−${formatUsdgAssets((-value).toString())}`, tone: 'negative' };
  return { text: formatUsdgAssets('0'), tone: 'neutral' };
}

function formatPrice18(raw) {
  if (typeof raw !== 'string') return '—';
  const trimmed = raw.trim();
  if (/^\d+$/.test(trimmed)) {
    const units = BigInt(trimmed);
    if (units > 0n && units < 200_000_000n) {
      return (Number(200_000_000n - units) / 1e8).toFixed(4);
    }
    const num = Number(units) / 1e18;
    return Number.isFinite(num) ? num.toFixed(4) : '—';
  }
  const parsed = Number(trimmed);
  return Number.isFinite(parsed) ? parsed.toFixed(4) : '—';
}

function formatPlDxyQty(raw) {
  if (typeof raw !== 'string' || !/^-?\d+$/.test(raw)) return '—';
  const num = Math.abs(Number(BigInt(raw))) / 1e18;
  if (!Number.isFinite(num)) return '—';
  return `${num.toFixed(num >= 100 ? 1 : 2)} plDXY`;
}

function formatUtcShort(iso) {
  if (!iso) return '—';
  const date = new Date(iso);
  if (Number.isNaN(date.getTime())) return '—';
  const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
  const day = String(date.getUTCDate()).padStart(2, '0');
  const mon = months[date.getUTCMonth()];
  const year = String(date.getUTCFullYear());
  const hh = String(date.getUTCHours()).padStart(2, '0');
  const mm = String(date.getUTCMinutes()).padStart(2, '0');
  return `${day} ${mon} ${year}, ${hh}:${mm}`;
}

function computeRoePercent(pnlRaw, baseRaw) {
  if (typeof pnlRaw !== 'string' || typeof baseRaw !== 'string') return null;
  if (!/^-?\d+$/.test(pnlRaw) || !/^\d+$/.test(baseRaw)) return null;
  const base = BigInt(baseRaw);
  if (base <= 0n) return null;
  const pct = (Number(BigInt(pnlRaw)) / Number(base)) * 100;
  if (!Number.isFinite(pct)) return null;
  return `${pct >= 0 ? '+' : ''}${pct.toFixed(2)}%`;
}

function toneHex(tone) {
  if (tone === 'positive') return '#00ff99';
  if (tone === 'negative') return '#ff572d';
  return '#fff5f9';
}

function networkLabelForChain(chainId) {
  if (chainId === 42161) return 'Arbitrum One';
  if (chainId === 421614) return 'Arbitrum Sepolia';
  return chainId ? `Chain ${String(chainId)}` : 'Arbitrum';
}

function renderPletherLogo(x, y, scale) {
  return `<g transform="translate(${String(x)}, ${String(y)}) scale(${scale.toFixed(4)})">
    <path d="M178.41 206.91C194.343 206.91 207.26 193.993 207.26 178.06C207.26 162.127 194.343 149.21 178.41 149.21C162.477 149.21 149.56 162.127 149.56 178.06C149.56 193.993 162.477 206.91 178.41 206.91Z" fill="#FFF5F9"/>
    <path d="M65.13 72.82H0.27V137.64H65.13V72.82Z" fill="#FFF5F9"/>
    <path d="M65.4 145.64H0V210.47H65.4V145.64Z" fill="#FFF5F9"/>
    <path d="M137.99 145.64H73.13V210.47H137.99V145.64Z" fill="#FFF5F9"/>
    <path d="M137.98 0H73.13V64.82H137.98V0Z" fill="#FFF5F9"/>
    <path d="M65.13 0H0.28V64.82H65.13V0Z" fill="#FFF5F9"/>
    <path d="M210.84 0H145.98V64.82H210.84V0Z" fill="#FFF5F9"/>
    <path d="M210.84 72.82H145.98V137.64H210.84V72.82Z" fill="#FFF5F9"/>
    <path d="M73.13 72.82V137.64H137.99V72.82H73.13ZM105.56 134.08C89.63 134.08 76.71 121.17 76.71 105.23C76.71 89.29 89.63 76.39 105.56 76.39C121.49 76.39 134.41 89.3 134.41 105.23C134.41 121.16 121.49 134.08 105.56 134.08Z" fill="#FFF5F9"/>
  </g>`;
}

function pillWidth(label) {
  return Math.round(Math.max(104, label.length * 10.6 + 34));
}

function renderPill(x, y, label, bg, border, fg) {
  const width = pillWidth(label);
  return `<g>
    <rect x="${String(x)}" y="${String(y)}" width="${String(width)}" height="34" fill="${bg}" stroke="${border}" stroke-width="1.2" />
    <text x="${String(Math.round(x + width / 2))}" y="${String(y + 22)}" text-anchor="middle" fill="${fg}" font-family="${MONO_FONT}" font-size="14" font-weight="700" letter-spacing="1.2">${escapeXml(label)}</text>
  </g>`;
}

function fitStatFontSize(value, boxW, maxSize) {
  const usableW = Math.max(80, boxW - 40);
  const charCount = Math.max(1, String(value).length);
  const fitted = Math.floor(usableW / (charCount * 0.64));
  return Math.min(maxSize, Math.max(14, fitted));
}

function renderStatBoxes(stats, y) {
  const gap = 18;
  const totalW = 1080;
  const count = stats.length;
  const boxW = Math.floor((totalW - gap * (count - 1)) / count);
  return stats
    .map((stat, idx) => {
      const x = 60 + idx * (boxW + gap);
      const fontSize = fitStatFontSize(stat.value, boxW, 22);
      return `<g>
        <rect x="${String(x)}" y="${String(y)}" width="${String(boxW)}" height="92" fill="#3b212d" fill-opacity="0.72" stroke="#ffab96" stroke-opacity="0.28" stroke-width="1" />
        <text x="${String(x + 20)}" y="${String(y + 30)}" fill="#aa98a0" font-family="${SANS_FONT}" font-size="13" font-weight="700" letter-spacing="1.1">${escapeXml(stat.label.toUpperCase())}</text>
        <text x="${String(x + 20)}" y="${String(y + 66)}" fill="${stat.accent ?? '#fff5f9'}" font-family="${MONO_FONT}" font-size="${String(fontSize)}" font-weight="700">${escapeXml(stat.value)}</text>
      </g>`;
    })
    .join('');
}

function renderTierBadge(tier, cx, cy) {
  const officialPaths = getOfficialBadgePaths(tier);
  if (!officialPaths || !tier) return '';

  const palette = resolveTierPalette(tier);
  const size = 188;
  const scale = size / 166;
  const left = Math.round(cx - size / 2);
  const top = Math.round(cy - size / 2);

  return `<g>
    <circle cx="${String(cx)}" cy="${String(cy)}" r="108" fill="${palette.glow}" fill-opacity="0.14" />
    <circle cx="${String(cx)}" cy="${String(cy)}" r="98" fill="none" stroke="${palette.primary}" stroke-opacity="0.45" stroke-width="1.5" />
    <g transform="translate(${String(left)}, ${String(top)}) scale(${scale.toFixed(4)})">
      ${officialPaths}
    </g>
    <text x="${String(cx)}" y="${String(cy + 126)}" text-anchor="middle" fill="${palette.primary}" font-family="${MONO_FONT}" font-size="16" font-weight="700" letter-spacing="2.4">${escapeXml(String(tier).toUpperCase())} DIVISION</text>
  </g>`;
}

function formatCompactUsdValue(dollars) {
  if (!Number.isFinite(dollars)) return '—';
  const abs = Math.abs(dollars);
  const sign = dollars < 0 ? '-' : '';
  if (abs >= 1_000_000_000) return `${sign}${(abs / 1_000_000_000).toFixed(2)}B`;
  if (abs >= 1_000_000) return `${sign}${(abs / 1_000_000).toFixed(2)}M`;
  if (abs >= 1_000) return `${sign}${(abs / 1_000).toFixed(2)}K`;
  return `${sign}${abs.toFixed(2)}`;
}

function formatMonthDayYear(isoOrUnix) {
  if (isoOrUnix == null) return null;
  const date = typeof isoOrUnix === 'number' ? new Date(isoOrUnix * 1000) : new Date(isoOrUnix);
  if (Number.isNaN(date.getTime())) return null;
  const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
  return `${String(date.getUTCDate())} ${months[date.getUTCMonth()]} ${String(date.getUTCFullYear())}`;
}

function renderLifetimeBalanceChart(balancePoints, fallbackValues) {
  const boxX = 532;
  const boxY = 132;
  const boxW = 608;
  const boxH = 252;
  const plotX = boxX + 22;
  const plotY = boxY + 52;
  const plotW = boxW - 44;
  const plotH = 146;

  const normalized =
    Array.isArray(balancePoints) && balancePoints.length > 0
      ? [...balancePoints]
          .map((pt) => ({ ts: Number(pt?.timestamp), val: Number(pt?.balanceAssets) / 1_000_000 }))
          .filter((pt) => Number.isFinite(pt.ts) && Number.isFinite(pt.val))
          .sort((a, b) => a.ts - b.ts)
      : (Array.isArray(fallbackValues) ? fallbackValues : [])
          .filter((v) => Number.isFinite(v))
          .map((val, idx) => ({ ts: idx, val }));

  if (normalized.length < 2) {
    const emptySvg = `<g>
      <rect x="${String(boxX)}" y="${String(boxY)}" width="${String(boxW)}" height="${String(boxH)}" fill="#3b212d" fill-opacity="0.52" stroke="#ffab96" stroke-opacity="0.26" stroke-width="1" />
      <text x="${String(plotX)}" y="${String(boxY + 30)}" fill="#aa98a0" font-family="${SANS_FONT}" font-size="13" font-weight="700" letter-spacing="1.3">LIFE-LONG BALANCE</text>
      <text x="${String(Math.round(boxX + boxW / 2))}" y="${String(Math.round(boxY + boxH / 2 + 10))}" text-anchor="middle" fill="#aa98a0" font-family="${SANS_FONT}" font-size="15" font-weight="500">Awaiting life-long balance history</text>
    </g>`;
    return { svg: emptySvg, accent: '#ffab96', startLabel: null };
  }

  const start = normalized[0];
  const end = normalized[normalized.length - 1];
  const stroke = end.val < start.val ? '#ff572d' : '#00ff99';
  const vals = normalized.map((pt) => pt.val);
  const rawMin = Math.min(...vals);
  const rawMax = Math.max(...vals);
  const span = rawMax - rawMin;
  const pad = span === 0 ? Math.max(rawMax * 0.08, 10) : span * 0.14;
  const minVal = Math.max(0, rawMin - pad);
  const maxVal = rawMax + pad;
  const rangeVal = Math.max(maxVal - minVal, 1);

  const minTs = start.ts;
  const maxTs = end.ts;
  const tsSpan = Math.max(maxTs - minTs, 1);
  const useTimestamps = Array.isArray(balancePoints) && balancePoints.length >= 2 && maxTs > minTs;

  const coords = normalized.map((pt, idx) => {
    const xRatio = useTimestamps ? (pt.ts - minTs) / tsSpan : idx / (normalized.length - 1);
    const x = plotX + xRatio * plotW;
    const y = plotY + (1 - (pt.val - minVal) / rangeVal) * plotH;
    return { x, y };
  });

  const linePath = coords
    .map((pt, idx) =>
      idx === 0
        ? `M${pt.x.toFixed(1)},${pt.y.toFixed(1)}`
        : `H${pt.x.toFixed(1)} V${pt.y.toFixed(1)}`,
    )
    .join(' ');
  const areaPath = `${linePath} L${coords[coords.length - 1].x.toFixed(1)},${String(plotY + plotH)} L${coords[0].x.toFixed(1)},${String(plotY + plotH)} Z`;
  const baselineY = plotY + (1 - (start.val - minVal) / rangeVal) * plotH;
  const lastPt = coords[coords.length - 1];
  const pointDots =
    coords.length <= 45
      ? coords
          .slice(0, -1)
          .map(
            (pt) =>
              `<circle cx="${pt.x.toFixed(1)}" cy="${pt.y.toFixed(1)}" r="2.6" fill="#ffab96" fill-opacity="0.8" stroke="#250917" stroke-width="1" />`,
          )
          .join('')
      : '';

  const delta = end.val - start.val;
  const pctStr =
    start.val > 0
      ? `${delta >= 0 ? '+' : ''}${((delta / start.val) * 100).toFixed(2)}%`
      : null;
  const rangeHeader = `${formatCompactUsdValue(start.val)} → ${formatCompactUsdValue(end.val)} USDG`;
  const headerRight = pctStr ? `${pctStr} · ${rangeHeader}` : rangeHeader;

  const startDate = useTimestamps ? formatMonthDayYear(start.ts) : null;
  const endDate = useTimestamps ? formatMonthDayYear(end.ts) : null;

  const svg = `<g>
    <defs>
      <linearGradient id="acct-bal-area" x1="0" y1="0" x2="0" y2="1">
        <stop offset="0%" stop-color="${stroke}" stop-opacity="0.28" />
        <stop offset="100%" stop-color="${stroke}" stop-opacity="0.02" />
      </linearGradient>
    </defs>
    <rect x="${String(boxX)}" y="${String(boxY)}" width="${String(boxW)}" height="${String(boxH)}" fill="#3b212d" fill-opacity="0.52" stroke="#ffab96" stroke-opacity="0.26" stroke-width="1" />
    <text x="${String(plotX)}" y="${String(boxY + 30)}" fill="#aa98a0" font-family="${SANS_FONT}" font-size="13" font-weight="700" letter-spacing="1.3">LIFE-LONG BALANCE</text>
    <text x="${String(plotX + plotW)}" y="${String(boxY + 30)}" text-anchor="end" fill="${stroke}" font-family="${MONO_FONT}" font-size="13" font-weight="700">${escapeXml(headerRight)}</text>
    <line x1="${String(plotX)}" y1="${String(plotY)}" x2="${String(plotX + plotW)}" y2="${String(plotY)}" stroke="#ffab96" stroke-opacity="0.1" stroke-width="1" />
    <line x1="${String(plotX)}" y1="${String(Math.round(plotY + plotH / 2))}" x2="${String(plotX + plotW)}" y2="${String(Math.round(plotY + plotH / 2))}" stroke="#ffab96" stroke-opacity="0.1" stroke-width="1" />
    <line x1="${String(plotX)}" y1="${String(plotY + plotH)}" x2="${String(plotX + plotW)}" y2="${String(plotY + plotH)}" stroke="#ffab96" stroke-opacity="0.16" stroke-width="1" />
    <line x1="${String(plotX)}" y1="${baselineY.toFixed(1)}" x2="${String(plotX + plotW)}" y2="${baselineY.toFixed(1)}" stroke="#aa98a0" stroke-opacity="0.38" stroke-width="1" stroke-dasharray="4 4" />
    <path d="${areaPath}" fill="url(#acct-bal-area)" />
    <path d="${linePath}" fill="none" stroke="${stroke}" stroke-width="2.6" stroke-linejoin="round" stroke-linecap="round" />
    ${pointDots}
    <circle cx="${lastPt.x.toFixed(1)}" cy="${lastPt.y.toFixed(1)}" r="9" fill="${stroke}" fill-opacity="0.22" />
    <circle cx="${lastPt.x.toFixed(1)}" cy="${lastPt.y.toFixed(1)}" r="4.5" fill="${stroke}" />
    <text x="${String(plotX)}" y="${String(plotY + 14)}" fill="#aa98a0" font-family="${MONO_FONT}" font-size="11">High ${escapeXml(formatCompactUsdValue(rawMax))}</text>
    <text x="${String(plotX)}" y="${String(plotY + plotH - 6)}" fill="#aa98a0" font-family="${MONO_FONT}" font-size="11">Low ${escapeXml(formatCompactUsdValue(rawMin))}</text>
    <text x="${String(plotX)}" y="${String(boxY + boxH - 18)}" fill="#aa98a0" font-family="${MONO_FONT}" font-size="12">${escapeXml(startDate ?? 'Start')}</text>
    <text x="${String(plotX + plotW)}" y="${String(boxY + boxH - 18)}" text-anchor="end" fill="#aa98a0" font-family="${MONO_FONT}" font-size="12">${escapeXml(endDate ? `${endDate} · Live` : 'Live')}</text>
  </g>`;

  return { svg, accent: stroke, startLabel: startDate };
}

function hashAvatarSeed(input, salt) {
  let h = (0x811c9dc5 ^ salt) >>> 0;
  for (let i = 0; i < input.length; i += 1) {
    h ^= input.charCodeAt(i);
    h = Math.imul(h, 0x01000193) >>> 0;
  }
  h ^= h >>> 16;
  h = Math.imul(h, 0x85ebca6b) >>> 0;
  h ^= h >>> 13;
  h = Math.imul(h, 0xc2b2ae35) >>> 0;
  h ^= h >>> 16;
  return h >>> 0;
}

function nextAvatarPrng(state) {
  state.s = (state.s + 0x6d2b79f5) >>> 0;
  let t = state.s;
  t = Math.imul(t ^ (t >>> 15), t | 1) >>> 0;
  t ^= (t + Math.imul(t ^ (t >>> 7), t | 61)) >>> 0;
  return (t ^ (t >>> 14)) >>> 0;
}

function mirrorAvatarRotation(kind, rotation) {
  const r = ((rotation % 4) + 4) % 4;
  if (kind === 'quarter' || kind === 'triangle') {
    return r === 0 ? 1 : r === 1 ? 0 : r === 2 ? 3 : 2;
  }
  if (kind === 'half') {
    return r === 1 ? 3 : r === 3 ? 1 : r;
  }
  return r;
}

function renderBauhausAvatar(address) {
  if (!address || typeof address !== 'string') return '';
  const palette = ['#FF572D', '#FFAB96', '#F2CF71', '#34D399', '#F472B6', '#FFF5F9'];
  const kinds = ['square', 'quarter', 'quarter', 'circle', 'half', 'triangle', 'empty', 'empty'];
  const normalized = address.trim().toLowerCase();
  const rng = { s: (hashAvatarSeed(normalized, 0x517cc1b7) ^ (hashAvatarSeed(normalized, 0x9e3779b9) >>> 1)) >>> 0 };
  const pIdx = nextAvatarPrng(rng) % palette.length;
  const sIdx = (pIdx + 1 + (nextAvatarPrng(rng) % (palette.length - 1))) % palette.length;
  const primary = palette[pIdx];
  const secondary = palette[sIdx];
  const colors = [primary, primary, secondary, secondary, '#4E2538'];
  const cells = [];
  let filled = 0;

  for (let row = 0; row < 3; row += 1) {
    const lk = kinds[nextAvatarPrng(rng) % kinds.length];
    const lr = nextAvatarPrng(rng) % 4;
    const lc = colors[nextAvatarPrng(rng) % colors.length];
    const ck = kinds[nextAvatarPrng(rng) % kinds.length];
    const cr = (nextAvatarPrng(rng) % 2) * 2;
    const cc = colors[nextAvatarPrng(rng) % colors.length];
    if (lk !== 'empty') filled += 2;
    if (ck !== 'empty') filled += 1;
    cells.push({ row, col: 0, kind: lk, rotation: lr, color: lc });
    cells.push({ row, col: 1, kind: ck, rotation: cr, color: cc });
    cells.push({ row, col: 2, kind: lk, rotation: mirrorAvatarRotation(lk, lr), color: lc });
  }
  if (filled < 4) {
    cells[4].kind = 'circle';
    cells[4].color = primary;
    if (cells[0].kind === 'empty') {
      cells[0].kind = 'quarter';
      cells[0].rotation = 0;
      cells[0].color = secondary;
      cells[2].kind = 'quarter';
      cells[2].rotation = 1;
      cells[2].color = secondary;
    }
  }
  const shapes = cells
    .map((c) => {
      if (c.kind === 'empty') return '';
      const x = c.col * 8;
      const y = c.row * 8;
      const r = c.rotation % 4;
      if (c.kind === 'square') return `<rect x="${String(x)}" y="${String(y)}" width="8" height="8" fill="${c.color}" />`;
      if (c.kind === 'circle') return `<circle cx="${String(x + 4)}" cy="${String(y + 4)}" r="3.2" fill="${c.color}" />`;
      if (c.kind === 'half') {
        if (r === 0) return `<rect x="${String(x)}" y="${String(y)}" width="8" height="4" fill="${c.color}" />`;
        if (r === 1) return `<rect x="${String(x + 4)}" y="${String(y)}" width="4" height="8" fill="${c.color}" />`;
        if (r === 2) return `<rect x="${String(x)}" y="${String(y + 4)}" width="8" height="4" fill="${c.color}" />`;
        return `<rect x="${String(x)}" y="${String(y)}" width="4" height="8" fill="${c.color}" />`;
      }
      if (c.kind === 'quarter') {
        if (r === 0) return `<path d="M${String(x)},${String(y)} H${String(x + 8)} A8,8 0 0 1 ${String(x)},${String(y + 8)} Z" fill="${c.color}" />`;
        if (r === 1) return `<path d="M${String(x + 8)},${String(y)} V${String(y + 8)} A8,8 0 0 1 ${String(x)},${String(y)} Z" fill="${c.color}" />`;
        if (r === 2) return `<path d="M${String(x + 8)},${String(y + 8)} H${String(x)} A8,8 0 0 1 ${String(x + 8)},${String(y)} Z" fill="${c.color}" />`;
        return `<path d="M${String(x)},${String(y + 8)} V${String(y)} A8,8 0 0 1 ${String(x + 8)},${String(y + 8)} Z" fill="${c.color}" />`;
      }
      if (r === 0) return `<polygon points="${String(x)},${String(y)} ${String(x + 8)},${String(y)} ${String(x)},${String(y + 8)}" fill="${c.color}" />`;
      if (r === 1) return `<polygon points="${String(x)},${String(y)} ${String(x + 8)},${String(y)} ${String(x + 8)},${String(y + 8)}" fill="${c.color}" />`;
      if (r === 2) return `<polygon points="${String(x + 8)},${String(y)} ${String(x + 8)},${String(y + 8)} ${String(x)},${String(y + 8)}" fill="${c.color}" />`;
      return `<polygon points="${String(x)},${String(y)} ${String(x + 8)},${String(y + 8)} ${String(x)},${String(y + 8)}" fill="${c.color}" />`;
    })
    .filter(Boolean)
    .join('');
  return `<g transform="translate(60, 558)"><rect width="24" height="24" fill="#1B0711" />${shapes}<rect x="0.5" y="0.5" width="23" height="23" fill="none" stroke="#FFAB96" stroke-opacity="0.28" stroke-width="1" /></g>`;
}

function renderFrame({
  accentGlow,
  eyebrow,
  networkTag,
  footerLeftPrimary,
  footerLeftSecondary,
  footerAvatarAddress,
  footerRight,
  bodySvg,
}) {
  const hasAvatar = Boolean(footerAvatarAddress);
  const leftTextX = hasAvatar ? 96 : 60;
  const secondaryX = leftTextX + Math.min(400, String(footerLeftPrimary).length * 11 + 26);
  return `<svg xmlns="http://www.w3.org/2000/svg" width="${String(OG_CARD_WIDTH)}" height="${String(OG_CARD_HEIGHT)}" viewBox="0 0 ${String(OG_CARD_WIDTH)} ${String(OG_CARD_HEIGHT)}">
  <defs>
    <linearGradient id="bg-grad" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0%" stop-color="#19040f" />
      <stop offset="55%" stop-color="#250917" />
      <stop offset="100%" stop-color="#2f0d1e" />
    </linearGradient>
    <radialGradient id="ambient-glow" cx="80%" cy="18%" r="55%">
      <stop offset="0%" stop-color="${accentGlow}" stop-opacity="0.26" />
      <stop offset="100%" stop-color="${accentGlow}" stop-opacity="0" />
    </radialGradient>
    <pattern id="grid-lines" width="48" height="48" patternUnits="userSpaceOnUse">
      <path d="M 48 0 L 0 0 0 48" fill="none" stroke="#ffab96" stroke-opacity="0.06" stroke-width="1" />
    </pattern>
  </defs>
  <rect width="${String(OG_CARD_WIDTH)}" height="${String(OG_CARD_HEIGHT)}" fill="url(#bg-grad)" />
  <rect width="${String(OG_CARD_WIDTH)}" height="${String(OG_CARD_HEIGHT)}" fill="url(#grid-lines)" />
  <rect width="${String(OG_CARD_WIDTH)}" height="${String(OG_CARD_HEIGHT)}" fill="url(#ambient-glow)" />
  <rect x="20" y="20" width="1160" height="590" fill="none" stroke="#ffab96" stroke-opacity="0.28" stroke-width="1.5" />

  ${renderPletherLogo(60, 48, 0.21)}
  <text x="118" y="69" fill="#fff5f9" font-family="${SANS_FONT}" font-size="22" font-weight="700" letter-spacing="0.4">Plether Insights</text>
  <text x="118" y="90" fill="#ffab96" font-family="${MONO_FONT}" font-size="13" font-weight="700" letter-spacing="1.6">${escapeXml(eyebrow.toUpperCase())}</text>

  <text x="1140" y="75" text-anchor="end" fill="#d8cbd0" font-family="${MONO_FONT}" font-size="14" font-weight="700">${escapeXml(networkTag)}</text>

  ${bodySvg}

  <line x1="60" y1="538" x2="1140" y2="538" stroke="#ffab96" stroke-opacity="0.24" stroke-width="1" />
  ${hasAvatar ? renderBauhausAvatar(footerAvatarAddress) : ''}
  <text x="${String(leftTextX)}" y="576" fill="#fff5f9" font-family="${SANS_FONT}" font-size="18" font-weight="700">${escapeXml(footerLeftPrimary)}</text>
  ${footerLeftSecondary ? `<text x="${String(secondaryX)}" y="576" fill="#aa98a0" font-family="${MONO_FONT}" font-size="15" font-weight="700">${escapeXml(footerLeftSecondary)}</text>` : ''}
  <text x="1140" y="576" text-anchor="end" fill="#ffab96" font-family="${MONO_FONT}" font-size="15" font-weight="700">${escapeXml(footerRight)}</text>
</svg>`;
}

export function buildDefaultOgSvg() {
  const stats = [
    { label: 'Execution', value: 'Fill & Latency', accent: '#00ff99' },
    { label: 'Liquidity Ops', value: 'LP Forensics', accent: '#ffab96' },
    { label: 'Keepers', value: 'Permissionless', accent: '#f2cf71' },
    { label: 'Divisions', value: 'Weekly Tiers', accent: '#fff5f9' },
  ];
  const body = `
    ${renderPill(60, 142, 'PROTOCOL INTELLIGENCE', '#3b212d', '#ffab9666', '#fff5f9')}
    ${renderPill(324, 142, 'ARBITRUM PERPS', '#00ff9922', '#00ff9988', '#00ff99')}
    <text x="60" y="256" fill="#fff5f9" font-family="${SANS_FONT}" font-size="64" font-weight="700">Verifiable On-Chain</text>
    <text x="60" y="330" fill="#ffab96" font-family="${SANS_FONT}" font-size="64" font-weight="700">Perps &amp; LP Analytics</text>
    <text x="60" y="376" fill="#d8cbd0" font-family="${SANS_FONT}" font-size="21" font-weight="500">Execution receipts, LP epoch settlement audits, trader profiles, and Divisions.</text>
    ${renderStatBoxes(stats, 412)}
  `;
  return renderFrame({
    accentGlow: '#ff572d',
    eyebrow: 'Protocol, Trader & LP Analytics',
    networkTag: 'insights.plether.com',
    footerLeftPrimary: 'Plether Protocol',
    footerLeftSecondary: 'USDG Perpetuals',
    footerRight: 'insights.plether.com',
    bodySvg: body,
  });
}

export function buildOgCardSvg(data) {
  if (!data || data.kind === 'default') return buildDefaultOgSvg();

  const networkTag = data.releaseVersion
    ? `${data.networkName} · ${data.releaseVersion}`
    : data.networkName;

  if (data.kind === 'trade') {
    const pnl = formatSignedUsdg(data.realizedPnlAssets);
    const color = toneHex(pnl.tone);
    const roi = computeRoePercent(data.realizedPnlAssets, data.notionalAssets);
    const sideText = data.side ? String(data.side).toUpperCase() : 'PERP';
    const sideBg = data.side === 'short' ? '#ff572d26' : '#00ff9922';
    const sideFg = data.side === 'short' ? '#ff572d' : '#00ff99';
    const stats = [
      { label: 'Close price', value: data.price ? formatPrice18(data.price) : '—' },
      { label: 'Quantity closed', value: data.sizeDelta ? formatPlDxyQty(data.sizeDelta) : '—' },
      { label: 'Notional', value: data.notionalAssets ? formatCompactUsdg(data.notionalAssets) : '—' },
      { label: 'Executed (UTC)', value: formatUtcShort(data.occurredAt) },
    ];
    const body = `
      ${renderPill(60, 134, data.market || 'plDXY-PERP', '#3b212d', '#ffab9666', '#fff5f9')}
      ${renderPill(218, 134, `CLOSED ${sideText}`, sideBg, `${sideFg}88`, sideFg)}
      <text x="60" y="220" fill="#aa98a0" font-family="${SANS_FONT}" font-size="16" font-weight="700" letter-spacing="1.4">REALIZED PNL</text>
      <text x="60" y="308" fill="${color}" font-family="${MONO_FONT}" font-size="72" font-weight="700">${escapeXml(pnl.text)}</text>
      ${roi ? `<text x="60" y="358" fill="${color}" font-family="${MONO_FONT}" font-size="24" font-weight="700">${escapeXml(roi)} on notional</text>` : ''}
      ${renderStatBoxes(stats, 408)}
    `;
    const footerRight = data.txHash
      ? `tx ${shortAddress(data.txHash)} · insights.plether.com`
      : 'insights.plether.com';
    return renderFrame({
      accentGlow: color,
      eyebrow: 'Closed Position Receipt',
      networkTag,
      footerLeftPrimary: data.traderLabel,
      footerLeftSecondary: shortAddress(data.account),
      footerAvatarAddress: data.account,
      footerRight,
      bodySvg: body,
    });
  }

  if (data.kind === 'position') {
    const upnl = formatSignedUsdg(data.unrealizedPnlAssets);
    const color = data.liquidatable ? '#ff572d' : toneHex(upnl.tone);
    const roe = computeRoePercent(data.unrealizedPnlAssets, data.marginAssets);
    const sideUpper = String(data.side || 'open').toUpperCase();
    const sideFg = data.side === 'short' ? '#ff572d' : '#00ff99';
    const stats = [
      { label: 'Position size', value: formatPlDxyQty(data.size) },
      { label: 'Entry price', value: formatPrice18(data.entryPrice) },
      { label: 'Margin posted', value: formatCompactUsdg(data.marginAssets) },
      { label: 'Status', value: data.liquidatable ? 'Liquidatable' : 'Healthy', accent: data.liquidatable ? '#ff572d' : '#00ff99' },
    ];
    const body = `
      ${renderPill(60, 134, data.market || 'plDXY-PERP', '#3b212d', '#ffab9666', '#fff5f9')}
      ${renderPill(218, 134, `OPEN ${sideUpper}`, `${sideFg}22`, `${sideFg}88`, sideFg)}
      <text x="60" y="220" fill="#aa98a0" font-family="${SANS_FONT}" font-size="16" font-weight="700" letter-spacing="1.4">UNREALIZED PNL</text>
      <text x="60" y="308" fill="${color}" font-family="${MONO_FONT}" font-size="72" font-weight="700">${escapeXml(upnl.text)}</text>
      ${roe ? `<text x="60" y="358" fill="${color}" font-family="${MONO_FONT}" font-size="26" font-weight="700">${escapeXml(roe)} ROE</text>` : ''}
      ${renderStatBoxes(stats, 408)}
    `;
    return renderFrame({
      accentGlow: color,
      eyebrow: 'Live Perpetual Position',
      networkTag,
      footerLeftPrimary: data.traderLabel,
      footerLeftSecondary: shortAddress(data.account),
      footerAvatarAddress: data.account,
      footerRight: 'CfdEngineAccountLens · insights.plether.com',
      bodySvg: body,
    });
  }

  if (data.kind === 'division') {
    const tierPalette = resolveTierPalette(data.tier);
    const rankHeadline = data.rank !== null && data.rank !== undefined ? `RANK #${String(data.rank)}` : 'ACTIVE TRADER';
    const returnLabel = String(data.weeklyReturnLabel ?? '0.00%');
    const returnColor = returnLabel.startsWith('-') || returnLabel.startsWith('−')
      ? '#ff572d'
      : '#00ff99';
    const pnlText = data.weeklyPnlAssets ? formatSignedUsdg(data.weeklyPnlAssets).text : null;
    const stats = [
      pnlText
        ? { label: 'Weekly PnL', value: pnlText, accent: returnColor }
        : { label: 'Status', value: data.provisional ? 'Provisional' : 'Published' },
      { label: 'Active days', value: `${String(data.activeDays ?? 0)} / 5` },
      { label: 'Tier standing', value: data.tier ?? 'Not yet awarded', accent: tierPalette.primary },
    ];
    const badgeTag = data.movement && data.movement !== 'unchanged'
      ? String(data.movement).toUpperCase()
      : data.provisional
        ? 'LIVE WEEK'
        : 'PUBLISHED';
    const weekLabel = data.weekLabel || 'Current Week';
    const body = `
      ${renderPill(60, 134, weekLabel, '#3b212d', '#ffab9666', '#fff5f9')}
      ${renderPill(60 + pillWidth(weekLabel) + 14, 134, badgeTag, `${tierPalette.primary}22`, `${tierPalette.primary}88`, tierPalette.primary)}
      <text x="60" y="226" fill="#aa98a0" font-family="${SANS_FONT}" font-size="16" font-weight="700" letter-spacing="1.4">PLETHER DIVISIONS</text>
      <text x="60" y="308" fill="#fff5f9" font-family="${MONO_FONT}" font-size="72" font-weight="700">${escapeXml(rankHeadline)}</text>
      <text x="60" y="358" fill="${returnColor}" font-family="${MONO_FONT}" font-size="28" font-weight="700">${escapeXml(returnLabel)} weekly return</text>
      ${renderTierBadge(data.tier, 990, 244)}
      ${renderStatBoxes(stats, 408)}
    `;
    const eyebrow = data.tier ? 'Plether Divisions Badge' : 'Plether Divisions Live Standing';
    return renderFrame({
      accentGlow: tierPalette.glow,
      eyebrow,
      networkTag: data.networkName,
      footerLeftPrimary: data.traderLabel,
      footerLeftSecondary: shortAddress(data.account),
      footerAvatarAddress: data.account,
      footerRight: 'insights.plether.com/divisions',
      bodySvg: body,
    });
  }

  // Trading account overview card (distinct from Divisions card — centers life-long balance curve)
  const pnl = data.realizedPnlAssets
    ? formatSignedUsdg(data.realizedPnlAssets)
    : { text: 'Unavailable', tone: 'neutral' };
  const pnlColor = toneHex(pnl.tone);
  const chart = renderLifetimeBalanceChart(data.balancePoints, data.sparklineValues);
  const glowAccent = pnl.tone !== 'neutral' ? pnlColor : chart.accent;
  const effectiveBalance =
    data.accountBalanceAssets ??
    (data.netEquityAssets && data.netEquityAssets !== '0' ? data.netEquityAssets : null) ??
    (Array.isArray(data.balancePoints) && data.balancePoints.length > 0
      ? data.balancePoints[data.balancePoints.length - 1]?.balanceAssets
      : null) ??
    data.netEquityAssets ??
    null;
  const stats = [
    { label: 'Balance', value: effectiveBalance ? formatCompactUsdg(effectiveBalance) : 'Unavailable' },
    { label: 'Volume', value: data.volumeAssets ? formatCompactUsdg(data.volumeAssets) : 'Unavailable' },
    { label: 'Executed fills', value: data.totalTrades != null ? Number(data.totalTrades).toLocaleString('en-US') : 'Unavailable' },
    { label: 'Active days', value: data.activeDays != null ? String(data.activeDays) : 'Unavailable' },
  ];
  const accountTierPalette = resolveTierPalette(data.tier);
  const pnlFontSize = pnl.text.length <= 13 ? 46 : pnl.text.length <= 16 ? 40 : 34;
  const sinceText = formatMonthDayYear(data.firstActivityAt) ?? chart.startLabel;
  const body = `
    ${renderPill(60, 132, 'TRADING ACCOUNT', '#3b212d', '#ffab9666', '#fff5f9')}
    ${data.tier ? renderPill(264, 132, `${String(data.tier).toUpperCase()} TIER`, `${accountTierPalette.primary}22`, `${accountTierPalette.primary}88`, accountTierPalette.primary) : ''}
    <text x="60" y="216" fill="#aa98a0" font-family="${SANS_FONT}" font-size="15" font-weight="700" letter-spacing="1.4">ALL-TIME REALIZED PNL</text>
    <text x="60" y="282" fill="${pnlColor}" font-family="${MONO_FONT}" font-size="${String(pnlFontSize)}" font-weight="700">${escapeXml(pnl.text)}</text>
    <text x="60" y="328" fill="#d8cbd0" font-family="${SANS_FONT}" font-size="17" font-weight="500">${escapeXml(sinceText ? `Life-long history since ${sinceText}` : 'Life-long on-chain perps performance')}</text>
    ${effectiveBalance ? `<text x="60" y="362" fill="#aa98a0" font-family="${MONO_FONT}" font-size="14" font-weight="700">Current balance · ${escapeXml(formatCompactUsdg(effectiveBalance))}</text>` : ''}
    ${chart.svg}
    ${renderStatBoxes(stats, 412)}
  `;
  return renderFrame({
    accentGlow: glowAccent,
    eyebrow: 'Trading Account Summary',
    networkTag,
    footerLeftPrimary: data.traderLabel,
    footerLeftSecondary: shortAddress(data.account),
    footerAvatarAddress: data.account,
    footerRight: 'insights.plether.com/wallets',
    bodySvg: body,
  });
}

export function selectWalletOgCardData(summaryPayload, { tradeId = null, cardMode = null } = {}) {
  const data = summaryPayload?.data ?? summaryPayload;
  if (!data || typeof data.account !== 'string') return null;

  const account = data.account.toLowerCase();
  const handle = typeof data.identity?.discordUsername === 'string' && data.identity.discordUsername.trim()
    ? `@${data.identity.discordUsername.trim()}`
    : shortAddress(account);
  const networkName = networkLabelForChain(data.network?.chainId);
  const releaseVersion = data.network?.releaseVersion ?? null;

  if (tradeId && Array.isArray(data.activity)) {
    const match = data.activity.find(
      (row) => row && row.id === tradeId && row.activityType === 'Close' && row.pnlAssets != null,
    );
    if (match) {
      return {
        kind: 'trade',
        traderLabel: handle,
        account,
        networkName,
        releaseVersion,
        market: 'plDXY-PERP',
        side: match.side === 'long' || match.side === 'short' ? match.side : null,
        realizedPnlAssets: String(match.pnlAssets),
        price: match.price ?? null,
        sizeDelta: match.sizeDelta ?? null,
        notionalAssets: match.amountAssets ?? null,
        occurredAt: match.occurredAt ?? '',
        txHash: match.txHash ?? null,
      };
    }
  }

  if (cardMode === 'position' && data.ledger?.hasPosition && data.ledger?.position) {
    const pos = data.ledger.position;
    return {
      kind: 'position',
      traderLabel: handle,
      account,
      networkName,
      releaseVersion,
      market: pos.market || 'plDXY-PERP',
      side: pos.side || 'unknown',
      size: String(pos.size ?? '0'),
      entryPrice: String(pos.entryPrice ?? '0'),
      marginAssets: String(pos.marginAssets ?? '0'),
      unrealizedPnlAssets: String(pos.unrealizedPnlAssets ?? '0'),
      liquidatable: Boolean(pos.liquidatable),
    };
  }

  const latestDiv = Array.isArray(data.divisionsHistory) ? data.divisionsHistory[0] : null;
  const seenKeys = new Set();
  const balancePoints = [];
  if (Array.isArray(data.balanceHistory)) {
    for (const pt of data.balanceHistory) {
      if (!pt || !Number.isFinite(Number(pt.timestamp)) || pt.balanceAssets == null) continue;
      const ts = Number(pt.timestamp);
      if (pt.source === 'receipt') {
        seenKeys.add(`${String(pt.blockNumber ?? '')}:${String(ts)}:${String(pt.eventType ?? '')}`);
      }
      balancePoints.push({
        timestamp: ts,
        balanceAssets: String(pt.balanceAssets),
      });
    }
  }
  if (Array.isArray(data.activity)) {
    for (const row of data.activity) {
      const receipt = row?.execution?.receipt;
      if (!receipt?.postSettlementBalanceAssets || !/^-?\d+$/.test(String(receipt.postSettlementBalanceAssets))) {
        continue;
      }
      const tsMs = Date.parse(row.occurredAt ?? '');
      if (!Number.isFinite(tsMs)) continue;
      const ts = Math.floor(tsMs / 1000);
      const key = `${String(row.blockNumber ?? '')}:${String(ts)}:${String(row.activityType ?? '')}`;
      if (seenKeys.has(key)) continue;
      seenKeys.add(key);
      const settle = BigInt(receipt.postSettlementBalanceAssets);
      const claim =
        receipt.postTraderClaimBalanceAssets && /^-?\d+$/.test(String(receipt.postTraderClaimBalanceAssets))
          ? BigInt(receipt.postTraderClaimBalanceAssets)
          : 0n;
      const total = settle + claim < 0n ? 0n : settle + claim;
      balancePoints.push({
        timestamp: ts,
        balanceAssets: total.toString(),
      });
    }
  }
  balancePoints.sort((a, b) => a.timestamp - b.timestamp);
  const sparklineValues = balancePoints
    .map((pt) => Number(pt.balanceAssets) / 1_000_000)
    .filter((n) => Number.isFinite(n));
  const latestHistoryBalance = balancePoints[balancePoints.length - 1]?.balanceAssets ?? null;
  const accountBalanceAssets =
    data.ledger?.settlementBalanceAssets ??
    (data.ledger?.netEquityAssets && data.ledger.netEquityAssets !== '0'
      ? data.ledger.netEquityAssets
      : null) ??
    latestHistoryBalance;

  return {
    kind: 'account',
    traderLabel: handle,
    account,
    networkName,
    releaseVersion,
    netEquityAssets: data.ledger?.netEquityAssets ?? null,
    accountBalanceAssets,
    realizedPnlAssets: data.performance?.realizedPnlAssets ?? null,
    volumeAssets: data.performance?.volumeAssets ?? null,
    totalTrades: data.performance?.totalTrades ?? null,
    activeDays: data.performance?.activeDays ?? null,
    firstActivityAt: data.performance?.firstActivityAt ?? null,
    tier: latestDiv?.tier ?? null,
    balancePoints,
    sparklineValues,
  };
}

function formatWeekLabel(isoOrUnix) {
  if (!isoOrUnix) return 'Current Week';
  const date = typeof isoOrUnix === 'number' ? new Date(isoOrUnix * 1000) : new Date(isoOrUnix);
  if (Number.isNaN(date.getTime())) return 'Current Week';
  const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
  return `Week of ${String(date.getUTCDate()).padStart(2, '0')} ${months[date.getUTCMonth()]} ${String(date.getUTCFullYear())}`;
}

function formatReturnPct(pnlRaw, equityRaw) {
  if (typeof pnlRaw !== 'string' || typeof equityRaw !== 'string') return '0.00%';
  if (!/^-?\d+$/.test(pnlRaw) || !/^\d+$/.test(equityRaw)) return '0.00%';
  const eq = BigInt(equityRaw);
  if (eq <= 0n) return '0.00%';
  const pct = (Number(BigInt(pnlRaw)) / Number(eq)) * 100;
  if (!Number.isFinite(pct)) return '0.00%';
  return `${pct >= 0 ? '+' : ''}${pct.toFixed(2)}%`;
}

export function selectDivisionOgCardData(account, livePayload, publishedPayload, summaryPayload) {
  const lower = String(account ?? '').toLowerCase();
  const live = livePayload?.data ?? livePayload;
  const pub = publishedPayload?.data ?? publishedPayload;
  const summary = summaryPayload?.data ?? summaryPayload;

  const liveStandings = Array.isArray(live?.standings) ? live.standings : [];
  const pubStandings = Array.isArray(pub?.standings) ? pub.standings : [];

  const liveEntry = liveStandings.find((s) => typeof s?.address === 'string' && s.address.toLowerCase() === lower);
  const pubEntry = pubStandings.find((s) => typeof s?.address === 'string' && s.address.toLowerCase() === lower);
  const summaryWeek = Array.isArray(summary?.divisionsHistory) ? summary.divisionsHistory[0] : null;

  if (!liveEntry && !pubEntry && !summaryWeek) return null;

  const rawHandle =
    liveEntry?.displayName ||
    pubEntry?.displayName ||
    summary?.identity?.discordUsername ||
    summaryWeek?.displayName ||
    null;
  const traderLabel = rawHandle && String(rawHandle).trim()
    ? (String(rawHandle).trim().startsWith('@') ? String(rawHandle).trim() : `@${String(rawHandle).trim()}`)
    : shortAddress(lower);

  const chainId = live?.chainId ?? pub?.chainId ?? summary?.network?.chainId ?? 421614;
  const networkName = networkLabelForChain(chainId);

  // Awarded tier comes ONLY from published standings (never live provisional week).
  const awardedTier = pubEntry?.tier ?? summaryWeek?.tier ?? null;

  if (liveEntry) {
    const returnVal = typeof liveEntry.weeklyReturn === 'number' && Number.isFinite(liveEntry.weeklyReturn)
      ? `${liveEntry.weeklyReturn >= 0 ? '+' : ''}${(liveEntry.weeklyReturn * 100).toFixed(2)}%`
      : formatReturnPct(liveEntry.weeklyPnlAssets, liveEntry.adjustedEquityAssets);
    return {
      kind: 'division',
      traderLabel,
      account: lower,
      networkName,
      tier: awardedTier,
      rank: typeof liveEntry.rank === 'number' ? liveEntry.rank : null,
      weeklyReturnLabel: returnVal,
      weeklyPnlAssets: liveEntry.weeklyPnlAssets ?? null,
      activeDays: typeof liveEntry.activeDays === 'number' ? liveEntry.activeDays : 0,
      movement: pubEntry?.movement ?? null,
      weekLabel: formatWeekLabel(live?.week?.startsAt ?? live?.weekStart),
      provisional: true,
    };
  }

  const ref = pubEntry ?? summaryWeek;
  const returnVal = typeof ref.weeklyReturn === 'number' && Number.isFinite(ref.weeklyReturn)
    ? `${ref.weeklyReturn >= 0 ? '+' : ''}${(ref.weeklyReturn * 100).toFixed(2)}%`
    : formatReturnPct(ref.weeklyPnlAssets, ref.adjustedEquityAssets);

  return {
    kind: 'division',
    traderLabel,
    account: lower,
    networkName,
    tier: awardedTier,
    rank: typeof ref.rank === 'number' ? ref.rank : null,
    weeklyReturnLabel: returnVal,
    weeklyPnlAssets: ref.weeklyPnlAssets ?? null,
    activeDays: typeof ref.activeDays === 'number' ? ref.activeDays : 0,
    movement: ref.movement ?? null,
    weekLabel: formatWeekLabel(pub?.week?.startsAt ?? summaryWeek?.weekStartsAt ?? summaryWeek?.weekStart),
    provisional: false,
  };
}
