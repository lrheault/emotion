import java.io.*;
import java.util.*;
import edu.stanford.nlp.io.*;
import edu.stanford.nlp.ling.*;
import edu.stanford.nlp.pipeline.*;
import edu.stanford.nlp.trees.*;
import edu.stanford.nlp.trees.TreeCoreAnnotations.*;
import edu.stanford.nlp.util.*;

/* 

 A Java wrapper for Stanford CoreNLP producing a CoNLL output with line and sentence numbering.
 
 Requirements:
 - openjdk 1.8 (Java v.8)
 - Stanford CoreNLP (v.2015-04-20) recompiled with our customized CoNLLOutputter.java file.
 
 Description:
 Pre-processes a corpus of text in which meaningful sections of text (e.g. speeches/Tweets/reviews) 
 appear on separate lines and are possibly contained across multiple files.
  
 Output format is a CoNLL (one-word-per-line) tab-separated file with columns:
 lineid	sentenceid	wordid	word	lemma	POS
 1		 	1			1	  	The		the		DT
 ...

 where:
 lineid 		-> the original line number (e.g. the speech number)
 sentenceid 	-> the sentence numbering within each lineid
 wordid 		-> the word numbering within each sentenceid.

 Example usage:
 java -cp "*:." -Xmx16g CoNLLSetup [batch-file] [input-dir] [output-dir]
 
 where:
 [batch-file] 	-> a file naming all files to be processed, for instance as obtained by:
 				   cd input-dir ; ls > batch-file 
 [input-dir] 	-> directory containing original corpus files
 [output-dir] 	-> output directory for CoNLL files.

 Author: L. Rheault

*/


public class CoNLLSetup {
	public static void main(String[] args) throws IOException {

		// Set the desired properties for CoreNLP.
		Properties props = new Properties();
		props.setProperty("annotators",
			"tokenize, ssplit, pos, lemma");
        // Set multi-threading to 6 (modify if needed).
        props.put("nthreads", "6");
        StanfordCoreNLP pipeline = new StanfordCoreNLP(props);

		Iterable<String> documents = IOUtils.readLines(args[0]);

		for (String document : documents) {
        	Iterable<String> lines = IOUtils.readLines(args[1]+"/"+document);
			int lineNumber = 1;
			for (String line : lines) {
				Annotation annotation = new Annotation(line);
				pipeline.annotate(annotation);
            	for (CoreMap sentence : annotation.get(CoreAnnotations.SentencesAnnotation.class)) {
                	sentence.set(CoreAnnotations.LineNumberAnnotation.class,lineNumber);
            	}
			lineNumber += 1;
			PrintWriter out = new PrintWriter(new FileOutputStream(new File(args[2]+"/"+document+"-output"),true));
			pipeline.conllPrint(annotation, out);
            out.close();
			}
		}
	}
}

